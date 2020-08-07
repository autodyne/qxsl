/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import qxsl.field.FieldFormat;
import qxsl.model.Field;

import javax.xml.namespace.QName;

/**
 * 交信の備考を表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Note extends Qxsl<String> {
	private final String note;

	/**
	 * 備考を指定して{@link Note}を構築します。
	 *
	 * @param note 備考
	 */
	public Note(String note) {
		super(NOTE);
		this.note = note;
	}

	@Override
	public String value() {
		return note;
	}

	/**
	 * {@link Note}を生成する書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/08
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName target() {
			return NOTE;
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
