/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.adif;

import java.time.ZoneOffset;
import java.time.LocalDate;
import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.table.FieldFormat;

import static java.time.format.DateTimeFormatter.BASIC_ISO_DATE;

/**
 * 交信の協定世界時の日付を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/28
 *
 */
public final class Date extends Adif<LocalDate> {
	private final LocalDate date;

	/**
	 * 協定世界時の日付を指定して{@link Date}を構築します。
	 * 
	 * @param date 日付
	 */
	public Date(LocalDate date) {
		super(DATE);
		this.date = date;
	}

	/**
	 * 協定世界時の日付を返します。
	 *
	 * @return 日付
	 */
	@Override
	public LocalDate value() {
		return date;
	}

	/**
	 * {@link Date}を生成するフォーマットです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/09
	 *
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName name() {
			return DATE;
		}

		@Override
		public Date decode(String value) {
			return new Date(LocalDate.parse(value, BASIC_ISO_DATE));
		}

		@Override
		public String encode(Field field) {
			return ((Date) field).value().format(BASIC_ISO_DATE);
		}
	}
}
