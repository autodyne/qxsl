/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.List;

import qxsl.model.Item;

/**
 * コンテストの部門はこのクラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Section extends Library {
	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 名前
	 */
	@Override
	public final String toString() {
		return name();
	}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 名前
	 */
	public abstract String name();

	/**
	 * 部門の番号を返します。
	 *
	 *
	 * @return 番号
	 */
	public abstract String code();

	/**
	 * キャッシュを返します。
	 *
	 *
	 * @return キャッシュ
	 *
	 * @since 2020/11/15
	 */
	public final Section cache() {
		return new Promise(this);
	}

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
	 * 指定された交信記録の識別子を発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 重複を除くための識別子
	 */
	public abstract Element unique(Item item);

	/**
	 * 指定された交信記録のマルチを発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 総得点を計算する識別子の配列
	 */
	public abstract Element entity(Item item);

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
	public abstract int result(Summary items);

	/**
	 * 指定された交信記録のマルチを集計します。
	 *
	 *
	 * @param list 交信記録
	 *
	 * @return 得点計算の結果
	 *
	 * @since 2019/05/16
	 */
	public final Summary summarize(List<Item> list) {
		return new Summary(this, list);
	}
}
