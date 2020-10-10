/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Field;

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
	 * 備考を指定して属性を構築します。
	 *
	 *
	 * @param note 備考
	 */
	public Note(String note) {
		super(NOTE);
		this.note = note;
	}

	@Override
	public final String value() {
		return note;
	}
}
