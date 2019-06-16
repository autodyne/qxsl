/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.model.FieldFormat;
import qxsl.table.secret.QxmlFields;

/**
 * コンテストで相手局と交換するシリアル番号です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/09
 *
 */
public final class Code extends Field<String> {
	private final String value;

	/**
	 * シリアル番号を指定して{@link Code}を構築します。
	 * 
	 * @param code シリアル番号
	 */
	public Code(String code) {
		super(QxmlFields.CODE);
		this.value = code;
	}
	
	@Override
	public String value() {
		return value;
	}

	/**
	 * {@link Code}を生成するフォーマットです。
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
			return QxmlFields.CODE;
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
