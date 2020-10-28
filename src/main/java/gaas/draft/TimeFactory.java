/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.draft;

import javax.xml.namespace.QName;

import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.field.FieldFactory;
import qxsl.value.Field;

import static java.time.ZonedDateTime.parse;
import static java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME;

/**
 * 交信の現地時刻を表す属性を永続化する書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class TimeFactory implements FieldFactory {
	/**
	 * 対応する属性の名前を返します。
	 *
	 *
	 * @return 属性の名前
	 */
	@Override
	public final QName target() {
		return Qxsl.TIME;
	}

	/**
	 * 文字列から属性値の実体を読み取ります。
	 *
	 *
	 * @param value 属性値を表す文字列
	 *
	 * @return 生成された属性値
	 */
	@Override
	public final Field decode(String value) {
		return new Time(parse(value, ISO_ZONED_DATE_TIME));
	}

	/**
	 * 指定された属性値を文字列に変換します。
	 *
	 *
	 * @param field 永続化する属性値
	 *
	 * @return 文字列化された属性値
	 */
	@Override
	public final String encode(Field field) {
		return ((Time) field).value().format(ISO_ZONED_DATE_TIME);
	}
}
