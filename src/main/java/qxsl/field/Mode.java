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
 * 交信記録シートにおいて通信方式を表現します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Mode extends Field<String> {
	private final String mode;
	
	/**
	 * モード名を指定して{@link Mode}を構築します。
	 * 
	 * @param mode モード名
	 */
	public Mode(String mode) {
		super(QxmlFields.MODE);
		this.mode = mode;
	}

	@Override
	public String value() {
		return mode;
	}

	/**
	 * {@link Mode}を生成するフォーマットです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/09
	 *
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName type() {
			return QxmlFields.MODE;
		}
	
		@Override
		public Mode decode(String value) {
			return new Mode(value);
		}
	
		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
