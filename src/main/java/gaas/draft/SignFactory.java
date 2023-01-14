/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.draft;

import javax.xml.namespace.QName;

import qxsl.draft.Qxsl;
import qxsl.draft.Sign;
import qxsl.field.FieldFactory;
import qxsl.value.Field;

import static java.time.ZonedDateTime.parse;
import static java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME;

/**
 * 交信の照合結果を表す属性を永続化する書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/08
 */
public final class SignFactory implements FieldFactory {
	/**
	 * 対応する属性の名前を返します。
	 *
	 *
	 * @return 属性の名前
	 */
	@Override
	public final QName target() {
		return Qxsl.SIGN;
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
		return new Sign(parse(value, ISO_ZONED_DATE_TIME));
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
		return ((Sign) field).value().format(ISO_ZONED_DATE_TIME);
	}
}
