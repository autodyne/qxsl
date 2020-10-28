/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.List;

import qxsl.model.Item;

/**
 * コンテストの総得点の計算手順を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/28
 */
public abstract class Counter extends Library {
	/**
	 * 計算手順を構築します。
	 */
	public Counter() {}

	/**
	 * 指定された交信記録の総得点を計算します。
	 *
	 *
	 * @param items 交信記録
	 *
	 * @return 総得点
	 *
	 * @since 2020/02/26
	 */
	public abstract int total(Summary items);

	/**
	 * 指定された交信記録の妥当性を検査します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 承認された場合はtrue
	 */
	public abstract Message verify(Item item);

	/**
	 * 指定された交信記録から有効な交信を抽出します。
	 *
	 *
	 * @param items 交信記録
	 *
	 * @return 得点計算の結果
	 *
	 * @since 2019/05/16
	 */
	public final Summary summarize(List<Item> items) {
		return new Summary(this, items);
	}
}
