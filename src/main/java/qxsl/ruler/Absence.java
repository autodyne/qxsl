/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;

/**
 * 参加者が不参加の部門を表す特殊な実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/23
 */
public abstract class Absence extends Section {
	/**
	 * 指定された交信記録の妥当性を検査します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 検証結果
	 */
	@Override
	public final Message verify(Item item) {
		return new Failure(item, "N/A");
	}

	/**
	 * 指定された交信記録の識別子を発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 重複を除くための識別子
	 *
	 * @since 2020/11/02
	 */
	@Override
	public final Element unique(Item item) {
		return new Element();
	}

	/**
	 * 指定された交信記録のマルチを発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 総得点を計算する識別子の配列
	 *
	 * @since 2020/11/02
	 */
	@Override
	public final Element entity(Item item) {
		return new Element();
	}

	/**
	 * 指定された集計結果の総得点を計算します。
	 *
	 *
	 * @param items 集計結果
	 *
	 * @return 総得点
	 *
	 * @since 2020/02/26
	 */
	@Override
	public final int result(Summary items) {
		return 0;
	}
}
