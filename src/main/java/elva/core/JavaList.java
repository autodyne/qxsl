/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.function.UnaryOperator;

/**
 * LISP処理系内部で利用される{@link List}と互換のリスト構造です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/07/10
 */
public final class JavaList extends BaseList {
	private final List<?> data;
	private final int head;
	private final int tail;

	/**
	 * 内容を指定してリストを構築します。
	 *
	 * @param data 内容
	 */
	public JavaList(List<?> data) {
		this(data, 0, data.size());
	}

	/**
	 * 内容と範囲を指定して配列を構築します。
	 *
	 * @param data 内容
	 * @param head 先頭の位置
	 * @param tail 末尾の位置
	 */
	public JavaList(List<?> data, int head, int tail) {
		this.data = data;
		this.head = Math.min(head, data.size());
		this.tail = Math.min(tail, data.size());
	}

	/**
	 * このリストを要素の配列に変換します。
	 *
	 * @return 要素の配列
	 */
	@Override
	public final List<?> value() {
		return data.subList(head, tail);
	}

	/**
	 * このリストの先頭を返します。
	 *
	 * @return 先頭
	 */
	@Override
	public final ElvaNode head() {
		if(isEmpty()) return ElvaList.NIL;
		return ElvaNode.wrap(data.get(head));
	}

	/**
	 * このリストの後続を返します。
	 *
	 * @return 後続
	 */
	@Override
	public final BaseList tail() {
		return new JavaList(data, head + 1, tail);
	}

	/**
	 * 指定された位置で始まる部分リストを返します。
	 *
	 * @param skip 部分リストが始まる位置
	 * @return 部分リスト
	 */
	@Override
	public final BaseList drop(int skip) {
		return new JavaList(data, head + skip, tail);
	}

	/**
	 * 指定された位置で終わる部分リストを返します。
	 *
	 * @param size 部分リストが終わる位置
	 * @return 部分リスト
	 */
	@Override
	public final BaseList take(int size) {
		return new JavaList(data, head, head + size);
	}

	/**
	 * このリストの指定された位置の要素を返します。
	 *
	 * @param index 要素の位置
	 * @return 要素
	 */
	@Override
	public final ElvaNode get(int index) {
		return ElvaNode.wrap(data.get(head + index));
	}

	/**
	 * このリストの要素数を返します。
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
	 * @return イテレータ
	 */
	@Override
	public final Iterator<ElvaNode> iterator() {
		return new ArrayIt(this);
	}

	/**
	 * このリストの内容を別のリストに変換します。
	 *
	 * @param op 写像
	 * @return 変換されたリスト
	 */
	@Override
	public final JavaList map(UnaryOperator<ElvaNode> op) {
		int idx = 0;
		final var vals = new ElvaNode[this.size()];
		for(var v: this) vals[idx++] = op.apply(v);
		return new JavaList(Arrays.asList(vals));
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class LIST implements Implicit {
		@Override
		public final boolean support(Object value) {
			return value instanceof List;
		}

		@Override
		public final ElvaNode encode(Object value) {
			return new JavaList((List<?>) value);
		}
	}
}
