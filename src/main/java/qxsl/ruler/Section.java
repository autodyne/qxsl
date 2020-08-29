/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.function.Function;

import qxsl.model.Item;

/**
 * コンテストの部門の実装はこのクラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Section implements Function<Item, Message> {
	private Contest contest = null;

	/**
	 * 部門を構築します。
	 */
	protected Section() {}

	/**
	 * 部門の名前を返します。
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
	 * @return 部門の名前
	 */
	public abstract String getName();

	/**
	 * 部門の符牒を返します。
	 *
	 * @return 部門の符牒
	 */
	public abstract String getCode();

	/**
	 * この部門を有するコンテストを返します。
	 *
	 * @return 部門を内包するコンテスト
	 */
	public final Contest getContest() {
		return this.contest;
	}

	/**
	 * この部門を有するコンテストを設定します。
	 *
	 * @param test 部門を内包するコンテスト
	 */
	protected void setContest(Contest test) {
		this.contest = test;
	}

	/**
	 * 指定された{@link Item}の可否を検査します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 承認された場合はtrue
	 */
	public abstract Message apply(Item item);

	/**
	 * この部門に紐づけられた関数を実行します。
	 *
	 *
	 * @param name 関数の名前
	 * @param args 関数の引数
	 *
	 * @return 関数の値
	 *
	 * @since 2020/03/09
	 */
	public abstract Object invoke(String name, Object...args);

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
	public final Summary summarize(Iterable<Item> items) {
		final var acc = new ArrayList<Success>();
		final var rej = new ArrayList<Failure>();
		for(Item item: items) {
			final Message msg = this.apply(item);
			if(msg instanceof Success) acc.add((Success) msg);
			if(msg instanceof Failure) rej.add((Failure) msg);
		}
		return new Summary(acc, rej).confirm(getContest());
	}
}
