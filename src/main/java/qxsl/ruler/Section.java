/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import qxsl.model.Item;

/**
 * コンテストの部門の実装は{@link Section}クラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Section implements Function<Item, Message> {
	private final String name;
	private final String code;

	/**
	 * 指定された名前と符牒で部門を構築します。
	 *
	 * @param name 部門の名前
	 * @param code 部門の符牒
	 */
	public Section(String name, String code) {
		this.name = name;
		this.code = code;
	}

	/**
	 * 部門の名前を返します。
	 *
	 * @return 部門の名前
	 */
	public final String getName() {
		return name;
	}

	/**
	 * 部門の符牒を返します。
	 *
	 * @return 部門の符牒
	 */
	public final String getCode() {
		return code;
	}

	/**
	 * UIで表示するために部門名を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * 指定された{@link Item}の可否を検査します。
	 *
	 * @param item 検査対象の交信記録
	 * @return 承認された場合はtrue
	 */
	public abstract Message apply(Item item);

	/**
	 * 指定された交信記録から有効な交信を抽出します。
	 *
	 * @param items 交信記録
	 * @return 得点計算の結果
	 *
	 * @since 2019/05/16
	 */
	public final Summary summarize(List<Item> items) {
		final List<Success> acc = new ArrayList<>();
		final List<Failure> rej = new ArrayList<>();
		for(Item item: items) {
			final Message msg = this.apply(item);
			if(msg instanceof Success) acc.add((Success) msg);
			if(msg instanceof Failure) rej.add((Failure) msg);
		}
		return new Summary(acc, rej);
	}
}
