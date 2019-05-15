/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.*;
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
public final class Time extends Field<ZonedDateTime> {
	private final ZonedDateTime time;

	/**
	 * 現在時刻で{@link Time}を構築します。
	 */
	 public Time() {
		this(ZonedDateTime.now());
	 }

	/**
	 * 交信日時を指定して{@link Time}を構築します。
	 * 
	 * @param time 交信日時
	 */
	public Time(Instant time) {
		this(time.atZone(ZoneId.systemDefault()));
	}

	/**
	 * 交信日時を指定して{@link Time}を構築します。
	 * 
	 * @param time 交信日時
	 */
	public Time(LocalDateTime time) {
		this(time.atZone(ZoneId.systemDefault()));
	}

	/**
	 * 交信日時を指定して{@link Time}を構築します。
	 * 
	 * @param time 交信日時
	 */
	public Time(ZonedDateTime time) {
		super(BaseFormat.TIME);
		this.time = time;
	}

	/**
	 * この交信の協定世界時における24時間制の時刻を返します。
	 *
	 * @return 交信時刻の時
	 */
	public int hour() {
		return time.withZoneSameInstant(ZoneOffset.UTC).getHour();
	}

	@Override
	public ZonedDateTime value() {
		return time;
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
		final Temporal comp = ((Time) obj).time;
		return comp.until(time, ChronoUnit.MINUTES) == 0;
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
		private final DateTimeFormatter format;

		public Format() {
			this.format = DateTimeFormatter.ISO_ZONED_DATE_TIME;
		}

		@Override
		public QName type() {
			return BaseFormat.TIME;
		}
	
		@Override
		public Time decode(String value) {
			return new Time(ZonedDateTime.parse(value, format));
		}
	
		@Override
		public String encode(Field field) {
			return ((Time) field).time.format(format);
		}
	}
}
