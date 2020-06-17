/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import javax.xml.namespace.QName;
import qxsl.field.FieldFormat;
import qxsl.model.Field;

/**
 * コンテストで相手局と交換するシリアル番号です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/09
 */
public final class Code extends Qxsl<String> {
	private final String value;

	/**
	 * シリアル番号を指定して{@link Code}を構築します。
	 *
	 * @param code シリアル番号
	 */
	public Code(String code) {
		super(CODE);
		this.value = code;
	}

	@Override
	public String value() {
		return value;
	}

	/**
	 * {@link Code}を生成する書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/08
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName target() {
			return CODE;
		}

		@Override
		public Code decode(String value) {
			return new Code(value);
		}

		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
