/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.Iterator;

/**
 * LISP処理系内部で利用される部分リスト構造の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class LimitSeq extends ListBase {
	private final ListBase base;
	private final int size;

	/**
	 * 内容と長さを指定してリストを構築します。
	 *
	 *
	 * @param base 内容
	 * @param size 長さ
	 */
	public LimitSeq(ListBase base, int size) {
		this.base = base;
		this.size = size;
	}

	/**
	 * このリストの先頭を返します。
	 *
	 *
	 * @return 先頭
	 */
	@Override
	public final NodeBase head() {
		return isEmpty()? NIL: base.head();
	}

	/**
	 * このリストの後続を返します。
	 *
	 *
	 * @return 後続
	 */
	@Override
	public final ListBase tail() {
		return new LimitSeq(base.tail(), size - 1);
	}

	/**
	 * 指定された位置で始まる部分リストを返します。
	 *
	 *
	 * @param skip 部分リストが始まる位置
	 *
	 * @return 部分リスト
	 */
	@Override
	public final ListBase drop(int skip) {
		return new LimitSeq(base.drop(skip), size - skip);
	}

	/**
	 * 指定された位置で終わる部分リストを返します。
	 *
	 *
	 * @param size 部分リストが終わる位置
	 *
	 * @return 部分リスト
	 */
	@Override
	public final ListBase take(int size) {
		return new LimitSeq(this, size);
	}

	/**
	 * このリストの指定された位置の要素を返します。
	 *
	 *
	 * @param index 要素の位置
	 *
	 * @return 要素
	 */
	@Override
	public final NodeBase get(int index) {
		if(index < size) return drop(index).head();
		final String msg = String.valueOf(index);
		throw new IndexOutOfBoundsException(msg);
	}

	/**
	 * このリストの要素数を返します。
	 *
	 *
	 * @return 要素数
	 */
	@Override
	public final int size() {
		return size;
	}

	/**
	 * リストの内容を列挙するイテレータを返します。
	 *
	 *
	 * @return イテレータ
	 */
	@Override
	public final Iterator<NodeBase> iterator() {
		return new ChainIt(this);
	}
}
