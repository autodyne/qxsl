/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.Iterator;

/**
 * LISP処理系内部で利用される配列リスト構造の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/03
 */
public final class ArraySeq extends ListBase {
	private final Object[] data;
	private final int head;
	private final int tail;

	/**
	 * 内容を指定してリストを構築します。
	 *
	 *
	 * @param data 内容
	 */
	public ArraySeq(Object[] data) {
		this(data, 0, data.length);
	}

	/**
	 * 内容と範囲を指定してリストを構築します。
	 *
	 *
	 * @param data 内容
	 * @param head 先頭の位置
	 * @param tail 末尾の位置
	 */
	private ArraySeq(Object[] data, int head, int tail) {
		this.data = data;
		this.head = Math.min(head, data.length);
		this.tail = Math.min(tail, data.length);
	}

	/**
	 * このリストの先頭を返します。
	 *
	 *
	 * @return 先頭
	 */
	@Override
	public final NodeBase head() {
		return isEmpty()? NIL: wrap(data[head]);
	}

	/**
	 * このリストの後続を返します。
	 *
	 *
	 * @return 後続
	 */
	@Override
	public final ListBase tail() {
		return new ArraySeq(data, head + 1, tail);
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
		return new ArraySeq(data, head + skip, tail);
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
		return new ArraySeq(data, head, head + size);
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
		return wrap(data[head + index]);
	}

	/**
	 * このリストの要素数を返します。
	 *
	 *
	 * @return 要素数
	 */
	@Override
	public final int size() {
		return tail - head;
	}

	/**
	 * このリストの内容をイテレータで返します。
	 *
	 *
	 * @return イテレータ
	 */
	@Override
	public final Iterator<NodeBase> iterator() {
		return new ArrayIt(this);
	}

	/**
	 * 指定された値の並びからリストを構築します。
	 *
	 *
	 * @param vals 内容
	 *
	 * @return リスト
	 */
	public static final ListBase from(Object...vals) {
		return new ArraySeq(vals);
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class LIST implements TypeRule {
		@Override
		public final boolean support(Object value) {
			return value instanceof Object[];
		}

		@Override
		public final NodeBase encode(Object value) {
			return new ArraySeq((Object[]) value);
		}
	}
}
