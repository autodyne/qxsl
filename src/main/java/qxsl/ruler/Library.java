/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.List;
import java.util.function.Function;

import qxsl.model.Item;

import static java.util.stream.Collectors.toList;

/**
 * ドメイン特化言語で定義された変数や関数を参照します。
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
	 * ライブラリの名前を返します。
	 *
	 *
	 * @return 名前
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * ライブラリの名前を返します。
	 *
	 *
	 * @return 名前
	 */
	public abstract String getName();

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
	 * 標準的な構造への変換処理を表現します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/10/25
	 */
	private final class Normalize implements Function<Item, Item> {
		private final String format;

		/**
		 * 変換前の交信の書式を指定します。
		 *
		 *
		 * @param format 変換前の書式
		 */
		public Normalize(String format) {
			this.format = format;
		}

		/**
		 * 指定された交信記録を処理します。
		 *
		 *
		 * @param item 交信記録
		 *
		 * @return 処理後の交信記録
		 */
		@Override
		public Item apply(Item item) {
			return normalize(item, this.format);
		}
	}

	/**
	 * 指定された書式の変換処理を表現します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/10/25
	 */
	private final class Transform implements Function<Item, Item> {
		private final String format;

		/**
		 * 変換後の交信の書式を指定します。
		 *
		 *
		 * @param format 変換後の書式
		 */
		public Transform(String format) {
			this.format = format;
		}

		/**
		 * 指定された交信記録を処理します。
		 *
		 *
		 * @param item 交信記録
		 *
		 * @return 処理後の交信記録
		 */
		@Override
		public Item apply(Item item) {
			return transform(item, this.format);
		}
	}

	/**
	 * 交信記録をライブラリが定義する標準構造に変換します。
	 * この関数は同名の関数を指定された引数で呼び出します。
	 *
	 *
	 * @param item 交信記録
	 * @param form 変換前の書式 nullを許容する
	 *
	 * @return 標準的な構造の交信記録
	 *
	 * @throws RuntimeException 関数の未定義または評価の例外
	 *
	 * @since 2020/09/04
	 */
	public final Item normalize(Item item, String form) {
		return (Item) invoke("normalize", item, form);
	}

	/**
	 * 交信記録を指定された書式に適合する構造に変換します。
	 * この関数は同名の関数を指定された引数で呼び出します。
	 *
	 *
	 * @param item 交信記録
	 * @param form 変換後の書式
	 *
	 * @return 書式に適合する交信記録
	 *
	 * @throws RuntimeException 関数の未定義または評価の例外
	 *
	 * @since 2020/09/04
	 */
	public final Item transform(Item item, String form) {
		return (Item) invoke("transform", item, form);
	}

	/**
	 * 交信記録をライブラリが定義する標準構造に変換します。
	 *
	 *
	 * @param list 交信記録
	 * @param form 変換前の書式 nullを許容する
	 *
	 * @return 標準的な構造の交信記録
	 *
	 * @since 2020/10/25
	 */
	public final List<Item> normalize(List<Item> list, String form) {
		return list.stream().map(new Normalize(form)).collect(toList());
	}

	/**
	 * 交信記録を指定された書式に適合する構造に変換します。
	 *
	 *
	 * @param list 交信記録
	 * @param form 変換後の書式
	 *
	 * @return 書式に適合する交信記録
	 *
	 * @since 2020/10/25
	 */
	public final List<Item> transform(List<Item> list, String form) {
		return list.stream().map(new Transform(form)).collect(toList());
	}
}
