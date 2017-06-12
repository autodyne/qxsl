/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.model.FieldFormat;
import qxsl.table.secret.BaseFormat;

import static java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME;

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
	public Time(Date time) {
		this(time.toInstant().atZone(ZoneId.systemDefault()));
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
	 * この交信の24時間制の時刻を返します。
	 *
	 * @return 交信時刻の時
	 */
	public int hour() {
		return time.get(ChronoField.HOUR_OF_DAY);
	}

	@Override
	public Date value() {
		return Date.from(time.toInstant());
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
		ZonedDateTime comp = ((Time) obj).time;
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
		@Override
		public QName type() {
			return BaseFormat.TIME;
		}
	
		@Override
		public Time decode(String value) {
			return new Time(ZonedDateTime.parse(value));
		}
	
		@Override
		public String encode(Field field) {
			return ((Time) field).time.format(ISO_ZONED_DATE_TIME);
		}
	}
}
