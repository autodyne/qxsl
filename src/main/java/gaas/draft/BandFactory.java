/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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
		return new Band(new BigDecimal(value));
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
}
