/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.UnaryOperator;

import qxsl.model.Item;

/**
 * 交信記録への処理の実装はこのクラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/26
 */
public abstract class Process implements UnaryOperator<Item> {
	/**
	 * 処理を構築します。
	 */
	public Process() {}

	/**
	 * 処理の名前を返します。
	 *
	 * @return {@link #name()}と同等
	 */
	@Override
	public final String toString() {
		return name();
	}

	/**
	 * 処理の名前を返します。
	 *
	 * @return 処理の名前
	 */
	public abstract String name();

	/**
	 * 指定された交信記録を処理します。
	 *
	 * @param item 処理対象の交信記録
	 * @return 処理の返り値
	 */
	public abstract Item apply(Item item);

	/**
	 * この処理に紐づけられた関数を実行します。
	 *
	 * @param name 関数の名前
	 * @param args 関数の引数
	 * @return 関数の値
	 *
	 * @since 2020/03/09
	 */
	public abstract Object invoke(String name, Object...args);

	/**
	 * 指定された交信記録に対して処理を実行します。
	 *
	 * @param items 交信記録
	 * @return 処理の結果
	 *
	 * @since 2020/03/09
	 */
	public final List<Item> process(Iterable<Item> items) {
		final var target = new ArrayList<Item>();
		for(var it: items) target.add(apply(it));
		return Collections.unmodifiableList(target);
	}
}
