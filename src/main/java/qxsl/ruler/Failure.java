/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;

/**
 * コンテスト規約に基づき無効な交信記録です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/26
 */
public final class Failure extends Message {
	private final String text;

	/**
	 * 交信記録と審査結果の文字列を設定します。
	 *
	 *
	 * @param item 交信の実体
	 * @param text 審査結果を説明する文字列
	 */
	public Failure(Item item, Object text) {
		super(item);
		this.text = String.valueOf(text);
	}

	/**
	 * 交信の成立で獲得する点数を返します。
	 *
	 *
	 * @return 交信1件の得点
	 */
	@Override
	public final int score() {
		return 0;
	}

	/**
	 * 採点結果を説明する文字列を返します。
	 *
	 *
	 * @return 文字列
	 */
	@Override
	public final String text() {
		return text;
	}
}
