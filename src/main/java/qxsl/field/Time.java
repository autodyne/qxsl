/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;
import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.model.FieldFormat;
import qxsl.table.secret.BaseFormat;

/**
 * 交信記録シートにおいて交信した日時を表現します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Time extends Field<Date> {
	private final Calendar time;

	/**
	 * 現在時刻で{@link Time}を構築します。
	 */
	 public Time() {
		this(new Date());
	 }

	/**
	 * 交信日時を指定して{@link Time}を構築します。
	 * 
	 * @param time 交信日時
	 */
	public Time(Date time) {
		super(BaseFormat.TIME);
		this.time = Calendar.getInstance();
		this.time.setTime(time);
	}

	/**
	 * この交信の24時間制の時刻を返します。
	 *
	 * @return 交信時刻の時
	 */
	public int hour() {
		return time.get(Calendar.HOUR_OF_DAY);
	}

	@Override
	public Date value() {
		return time.getTime();
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 * 対象が{@link Time}で、時刻が分まで等しい場合に、
	 * trueを返します。
	 * 
	 * @param obj 比較するオブジェクト
	 * @return この属性と等しい場合true
	 */
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof Time)) return false;
		final Time comp = (Time) obj;
		long mil1 = this.time.getTimeInMillis();
		long mil2 = comp.time.getTimeInMillis();
		return Math.abs(mil1 - mil2) < 60_000;
	}

	/**
	 * {@link Time}を生成するフォーマットです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/08
	 *
	 */
	public static final class Format implements FieldFormat {
		private final RFC3339 rfc3339 = new RFC3339();
		
		@Override
		public QName type() {
			return BaseFormat.TIME;
		}
	
		@Override
		public Time decode(String value) throws ParseException {
			return rfc3339.parse(value);
		}
	
		@Override
		public String encode(Field field) {
			return rfc3339.format((Time) field);
		}
	
		/**
		 * RFC3339形式による日時と文字列の相互変換を行います。
		 * 
		 * 
		 * @author Journal of Hamradio Informatics
		 * 
		 * @since 2013/02/21
		 *
		 */
		private static final class RFC3339 {
			static final String DECODE = "yyyy-MM-dd'T'HH:mm:ssZ";
			static final String ENCODE = "yyyy-MM-dd'T'HH:mm:ss";
			private final DateFormat decode;
			private final DateFormat encode;
			private final TimeZone timeZone;
			
			/**
			 * デフォルトのロケールでRFC3339を構築します。
			 */
			public RFC3339() {
				this(Locale.getDefault());
			}
			
			/**
			 * 指定したロケールでRFC3339を構築します。
			 * 
			 * @param locale システムのロケール
			 */
			public RFC3339(Locale locale) {
				decode = new SimpleDateFormat(DECODE, locale);
				encode = new SimpleDateFormat(ENCODE, locale);
				timeZone = encode.getTimeZone();
			}
			
			/**
			 * 指定された日時をRFC3339形式の文字列に変換します。
			 * 
			 * @param time 変換する日時
			 * @return RFC3339形式で日時を表現する文字列
			 */
			public String format(Time time) {
				Date date = time.value();
				String t = encode.format(date);
				long o = timeZone.getOffset(date.getTime());
				long h = o / 1000 / 60 / 60;
				long m = o / 1000 / 60 % 60;
				char s = o > 0L? '+' : '-';
				return String.format("%s%c%02d:%02d", t, s, h, m);
			}
			
			/**
			 * RFC3339形式の文字列を{@link Time}に変換します。
			 * 
			 * @param time 変換する文字列
			 * @return 対応する日時
			 * @throws ParseException RFC3339形式でない場合
			 */
			public Time parse(String time) throws ParseException {
				try {
					String t = time.substring(0, 19);
					String z = time.substring(19);
					StringBuilder sb = new StringBuilder(t);
					if(z.matches("[+-][0-9]{2}:?[0-9]{2}")) {
						sb.append(z.substring(0, 3));
						sb.append(z.substring(4, 6));
					} else {
						sb.append("+0000");
					}
					time = sb.toString();
					return new Time(decode.parse(time));
				} catch(IndexOutOfBoundsException ex) {
					String tmp = "'%s' is too short";
					String msg = String.format(tmp, time);
					throw new ParseException(msg, 0);
				}
			}
		}
	}
}
