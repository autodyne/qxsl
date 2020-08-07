/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.Iterator;

/**
 * LISP処理系で使用される単純型の共通実装です。
 *
 *
 * @param <V> 値の総称型
 *
 * @author 無線部開発班
 *
 * @since 2020/06/04
 */
public abstract class AtomBase<V> extends NodeBase {
	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 * @return 値
	 */
	public abstract V value();

	/**
	 * このアトムを表す文字列を返します。
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public abstract String toString();

	/**
	 * この式がアトムであるか確認します。
	 *
	 * @return アトムの場合は真
	 */
	@Override
	public final boolean isAtom() {
		return true;
	}

	/**
	 * このアトムの内容をイテレータで返します。
	 *
	 * @return イテレータ
	 */
	@Override
	public final Iterator<NodeBase> iterator() {
		return ListBase.list(this).iterator();
	}
}
