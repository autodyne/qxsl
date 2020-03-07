/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.Iterator;
import java.util.function.Function;
import qxsl.model.Item;

/**
 * 交信記録に対する手続きは{@link Handler}クラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/26
 */
public abstract class Handler implements Function<Item, Item> {
	/**
	 * 秘匿されたコンストラクタです。
	 *
	 * @since 2020/02/28
	 */
	Handler() {}

	/**
	 * 手続きの名前を返します。
	 *
	 * @return 手続きの名前
	 */
	public abstract String getName();

	/**
	 * UIで表示するために手続きの名前を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * 指定された交信記録を処理します。
	 *
	 * @param item 処理対象の交信記録
	 * @return 処理の返り値
	 *
	 * @throws RuntimeException 処理の過程で発生した何らかの例外
	 */
	public abstract Item apply(Item item) throws RuntimeException;
}
