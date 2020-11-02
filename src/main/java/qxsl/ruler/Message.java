/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Serializable;

import qxsl.model.Item;

/**
 * コンテスト規約に基づく交信の検証結果です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/12/05
 */
public abstract class Message implements Serializable {
	private final Item item;

	/**
	 * 交信の実体を設定します。
	 *
	 *
	 * @param item 交信記録
	 */
	public Message(Item item) {
		this.item = item;
	}

	/**
	 * 交信の実体を表す交信記録を返します。
	 *
	 *
	 * @return 交信の実体
	 */
	public final Item item() {
		return item;
	}

	/**
	 * 交信の成立で獲得する点数を返します。
	 *
	 *
	 * @return 交信1件の得点
	 */
	public abstract int score();

	/**
	 * 採点結果を説明する文字列を返します。
	 *
	 *
	 * @return 文字列
	 */
	public abstract String text();

	/**
	 * 規約に基づき有効な交信であるか確認します。
	 *
	 *
	 * @return 有効な交信の場合は真
	 */
	public final boolean isSuccess() {
		return this instanceof Success;
	}

	/**
	 * 規約に基づき無効な交信であるか確認します。
	 *
	 *
	 * @return 無効な交信の場合は真
	 */
	public final boolean isFailure() {
		return this instanceof Failure;
	}
}
