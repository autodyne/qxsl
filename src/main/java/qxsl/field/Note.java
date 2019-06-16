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
 * 交信記録シートにおいて備考を表現します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Note extends Field<String> {
	private final String note;
	
	/**
	 * 備考を指定して{@link Note}を構築します。
	 * 
	 * @param note 備考
	 */
	public Note(String note) {
		super(QxmlFields.NOTE);
		this.note = note;
	}

	@Override
	public String value() {
		return note;
	}

	/**
	 * {@link Note}を生成するフォーマットです。
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
			return QxmlFields.NOTE;
		}
	
		@Override
		public Note decode(String value) {
			return new Note(value);
		}
	
		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
