/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import javax.xml.namespace.QName;

import qxsl.field.FieldFormat;
import qxsl.field.FieldMapper;
import qxsl.model.Field;
import qxsl.model.Item;

/**
 * 交信の日時を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Time extends Qxsl<ZonedDateTime> {
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
		super(TIME);
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
	 * 対象が分まで同じ{@link Time}の場合にtrueを返します。
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
	 * {@link Time}を生成する書式です。
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
		public QName target() {
			return TIME;
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

	/**
	 * {@link Time}への変換を行う変換器です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/06/29
	 *
	 */
	public static final class Mapper implements FieldMapper {
		private final static String FORMAT = "yyyyMMddHHmm[ss]VV";
		private final DateTimeFormatter format;

		public Mapper() {
			this.format = DateTimeFormatter.ofPattern(FORMAT);
		}

		@Override
		public QName target() {
			return TIME;
		}

		/**
		 * 指定された日付と時刻から{@link Time}を生成します。
		 *
		 * @param date 日付の文字列 "20190629"
		 * @param time 時刻の文字列 "120030"
		 * @return 交信の日時
		 */
		private final Time adif(Object date, Object time) {
			String text = String.format("%s%sZ", date, time);
			return new Time(ZonedDateTime.parse(text, format));
		}

		@Override
		public Time search(Item item) {
			Object d = item.value(new QName(ADIF, "QSL_DATE"));
			Object t = item.value(new QName(ADIF, "TIME_ON"));
			return (d != null && t != null)? adif(d, t): null;
		}
	}
}
