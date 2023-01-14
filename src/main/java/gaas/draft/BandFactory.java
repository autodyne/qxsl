/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.draft;

import java.math.BigDecimal;
import javax.xml.namespace.QName;

import qxsl.draft.Band;
import qxsl.draft.Qxsl;
import qxsl.field.FieldFactory;
import qxsl.value.Field;

/**
 * 交信の周波数帯を表す属性を永続化する書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class BandFactory implements FieldFactory {
	/**
	 * 対応する属性の名前を返します。
	 *
	 *
	 * @return 属性の名前
	 */
	@Override
	public final QName target() {
		return Qxsl.BAND;
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
		return new Band(decodeBand(value));
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
		return ((Band) field).value().toPlainString();
	}

	/**
	 * 単位付き文字列を解析してキロヘルツ単位の値を返します。
	 *
	 *
	 * @param text 単位付き文字列 "1.9MHz"等
	 *
	 * @return キロヘルツ単位の波長
	 *
	 * @throws NumberFormatException 書式の例外
	 */
	private final BigDecimal decodeBand(String text) {
		final var digit = text.replaceAll("[kMG]?Hz$", "");
		final var value = new BigDecimal(digit);
		switch(text.substring(digit.length())) {
			case    "": return value;
			case  "Hz": return value.scaleByPowerOfTen(-3);
			case "kHz": return value.scaleByPowerOfTen(+0);
			case "MHz": return value.scaleByPowerOfTen(+3);
			case "GHz": return value.scaleByPowerOfTen(+6);
			default: throw new NumberFormatException(text);
		}
	}
}
