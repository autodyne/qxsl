/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Tuple;

/**
 * 交信記録の備考を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Note extends Qxsl<String> {
	/**
	 * 備考を指定して属性を構築します。
	 *
	 *
	 * @param note 備考
	 */
	public Note(String note) {
		super(NOTE, note);
	}

	/**
	 * 交信記録の備考を抽出します。
	 *
	 *
	 * @param tuple 交信記録
	 *
	 * @return 備考の属性
	 *
	 * @since 2020/10/28
	 */
	public static final Note from(Tuple tuple) {
		return (Note) tuple.get(Qxsl.NOTE);
	}
}
