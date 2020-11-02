/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;

/**
 * コンテスト規約に基づき有効な交信記録です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/26
 */
public final class Success extends Message {
	private final int mark;

	/**
	 * 交信記録と交信当たりの得点を設定します。
	 *
	 *
	 * @param item 交信の実体
	 * @param mark 得点
	 */
	public Success(Item item, int mark) {
		super(item);
		this.mark = mark;
	}

	/**
	 * 交信の成立で獲得する点数を返します。
	 *
	 *
	 * @return 交信1件の得点
	 */
	@Override
	public final int score() {
		return mark;
	}

	/**
	 * 採点結果を説明する文字列を返します。
	 *
	 *
	 * @return 文字列
	 */
	@Override
	public final String text() {
		return "";
	}
}
