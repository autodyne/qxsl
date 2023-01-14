/*******************************************************************************
 * Amateur Radio Operational Logging Pattern 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.ruler;

import java.util.List;

import qxsl.model.Item;

import static java.util.stream.Collectors.toList;

/**
 * 交信記録の標準構造と変換処理を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/02
 */
public abstract class Pattern extends Library {
	/**
	 * ライブラリを構築します。
	 */
	public Pattern() {}

	/**
	 * 相互に交信が成立した無線局間で交信記録を照合します。
	 *
	 *
	 * @param a 片方の無線局の交信記録
	 * @param b 他方の無線局の交信記録
	 *
	 * @return 照合の結果
	 *
	 * @since 2022/08/11
	 */
	public abstract boolean match(Item a, Item b);

	/**
	 * 交信記録をライブラリが定義する標準構造に変換します。
	 *
	 *
	 * @param item 交信記録
	 * @param type 変換前の書式 nullを許容する
	 *
	 * @return 標準的な構造の交信記録
	 *
	 * @since 2020/09/04
	 */
	public abstract Item normalize(Item item, String type);

	/**
	 * 交信記録を指定された書式に適合する構造に変換します。
	 *
	 *
	 * @param item 交信記録
	 * @param type 変換後の書式
	 *
	 * @return 書式に適合する交信記録
	 *
	 * @since 2020/09/04
	 */
	public abstract Item transform(Item item, String type);

	/**
	 * 交信記録をライブラリが定義する標準構造に変換します。
	 *
	 *
	 * @param list 交信記録
	 * @param type 変換前の書式 nullを許容する
	 *
	 * @return 標準的な構造の交信記録
	 *
	 * @since 2020/10/25
	 */
	public final List<Item> normalize(List<Item> list, String type) {
		return list.stream().map(i -> normalize(i, type)).collect(toList());
	}

	/**
	 * 交信記録を指定された書式に適合する構造に変換します。
	 *
	 *
	 * @param list 交信記録
	 * @param type 変換後の書式
	 *
	 * @return 書式に適合する交信記録
	 *
	 * @since 2020/10/25
	 */
	public final List<Item> transform(List<Item> list, String type) {
		return list.stream().map(i -> transform(i, type)).collect(toList());
	}
}
