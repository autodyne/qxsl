/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.List;

import qxsl.model.Item;
import qxsl.table.TableFactory;

/**
 * コンテストの部門の実装はこのクラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Section implements Library {
	private Contest contest = null;

	/**
	 * 部門を構築します。
	 */
	public Section() {}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 部門の名前
	 */
	public abstract String getName();

	/**
	 * 部門の符牒を返します。
	 *
	 *
	 * @return 部門の符牒
	 */
	public abstract String getCode();

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
	public abstract int score(Summary items);

	/**
	 * 指定された{@link Item}の可否を検査します。
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

	/**
	 * 規約に基づき交信記録を標準形式に変換して得点計算に備えます。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @return 得点計算が可能な標準形式の交信記録
	 *
	 * @since 2020/09/04
	 */
	public final Item decode(Item item) {
		return (Item) invoke("decode", item.clone());
	}

	/**
	 * 規約に基づき交信記録を特定の書式に対応した状態に変換します。
	 *
	 *
	 * @param item 交信記録
	 * @param form 書式
	 *
	 * @return 指定された書式で出力可能な交信記録
	 *
	 * @since 2020/09/04
	 */
	public final Item encode(Item item, TableFactory form) {
		return (Item) invoke("encode", item.clone(), form.getName());
	}
}
