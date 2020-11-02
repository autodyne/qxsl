/*******************************************************************************
 * Amateur Radio Operational Logging Pattern 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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
	 * 交信記録をライブラリが定義する標準構造に変換します。
	 *
	 *
	 * @param item 交信記録
	 * @param form 変換前の書式 nullを許容する
	 *
	 * @return 標準的な構造の交信記録
	 *
	 * @since 2020/09/04
	 */
	public abstract Item normalize(Item item, String form);

	/**
	 * 交信記録を指定された書式に適合する構造に変換します。
	 *
	 *
	 * @param item 交信記録
	 * @param form 変換後の書式
	 *
	 * @return 書式に適合する交信記録
	 *
	 * @since 2020/09/04
	 */
	public abstract Item transform(Item item, String form);

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
		return list.stream().map(i -> normalize(i, form)).collect(toList());
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
		return list.stream().map(i -> transform(i, form)).collect(toList());
	}
}
