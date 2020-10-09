/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;
import qxsl.table.TableFactory;

/**
 * ドメイン特化言語で定義された変数や関数の参照を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/26
 */
public abstract class Library {
	/**
	 * ライブラリを構築します。
	 */
	public Library() {}

	/**
	 * このライブラリが参照する変数を実行します。
	 *
	 *
	 * @param name 変数の名前
	 *
	 * @return 変数の値
	 *
	 * @since 2020/09/27
	 */
	public abstract Object get(String name);

	/**
	 * このライブラリが参照する関数を実行します。
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
		return (Item) invoke("decode", item);
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
		return (Item) invoke("encode", item, form.getName());
	}
}
