/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.draft;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import javax.xml.namespace.QName;

import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.field.FieldFactory;
import qxsl.value.Field;

/**
 * {@link Time}を生成する書式です。
 *
 * @author 無線部開発班
 * @since 2013/06/08
 */
public final class TimeFactory implements FieldFactory {
	private final DateTimeFormatter format;

	public TimeFactory() {
		this.format = DateTimeFormatter.ISO_ZONED_DATE_TIME;
	}

	@Override
	public QName target() {
		return Qxsl.TIME;
	}

	@Override
	public Time decode(String value) {
		return new Time(ZonedDateTime.parse(value, format));
	}

	@Override
	public String encode(Field field) {
		return ((Time) field).value().format(format);
	}
}
