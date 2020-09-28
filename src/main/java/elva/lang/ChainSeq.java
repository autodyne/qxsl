/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.Iterator;

/**
 * LISP処理系内部で利用される連鎖リスト構造の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class ChainSeq extends ListBase {
	private final NodeBase head;
	private final ListBase tail;
	private final int size;

	/**
	 * 連鎖する要素を指定してリストを構築します。
	 *
	 *
	 * @param head 先頭の要素
	 * @param tail 末尾の要素
	 */
	public ChainSeq(NodeBase head, ListBase tail) {
		this.head = head;
		this.tail = tail;
		this.size = tail.size() + 1;
	}

	/**
	 * このリストの先頭を返します。
	 *
	 *
	 * @return 先頭
	 */
	@Override
	public final NodeBase head() {
		return head;
	}

	/**
	 * このリストの後続を返します。
	 *
	 *
	 * @return 後続
	 */
	@Override
	public final ListBase tail() {
		return tail;
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
		if(skip == 0) return this;
		if(skip >= 1) return tail.drop(skip - 1);
		final String msg = String.valueOf(skip);
		throw new IndexOutOfBoundsException(msg);
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
	 * このリストの内容をイテレータで返します。
	 *
	 *
	 * @return イテレータ
	 */
	@Override
	public final Iterator<NodeBase> iterator() {
		return new ChainIt(this);
	}
}
