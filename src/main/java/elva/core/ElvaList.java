/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.function.UnaryOperator;

/**
 * LISP処理系内部で利用される配列と互換のリスト構造です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public abstract class ElvaList extends BaseList {
	/**
	 * 内容が空のリストを示す特別なインスタンスです。
	 */
	public static final EmptySeq NIL = new EmptySeq();

	/**
	 * このリストを要素の配列に変換します。
	 *
	 * @return 要素の配列
	 */
	@Override
	public final Object[] value() {
		return toArray(Object.class);
	}

	/**
	 * このリストの内容を別のリストに変換します。
	 *
	 * @param op 写像
	 * @return 変換されたリスト
	 */
	@Override
	public final BaseList map(UnaryOperator<ElvaNode> op) {
		int idx = 0;
		final var vals = new ElvaNode[this.size()];
		for(var v: this) vals[idx++] = op.apply(v);
		return new ArraySeq(vals);
	}

	/**
	 * 指定された値を要素に持つ配列リスト構造を構築します。
	 *
	 * @param vals 要素
	 * @return リスト
	 */
	public static final ElvaList array(Iterable<?> vals) {
		final var list = new ArrayList<Object>();
		for(var e: vals) list.add(e);
		return new ArraySeq(list.toArray());
	}

	/**
	 * 指定された値を要素に持つ連鎖リスト構造を構築します。
	 *
	 * @param vals 要素
	 * @return リスト
	 */
	public static final ElvaList chain(Iterable<?> vals) {
		ElvaList seq = NIL;
		final var list = new LinkedList<ElvaNode>();
		for(var e: vals) list.push(ElvaNode.wrap(e));
		for(var e: list) seq = new ChainSeq(e, seq);
		return seq;
	}

	/**
	 * 指定された値をリストに変換して返します。
	 * 既にリストである場合はそのまま返します。
	 *
	 * @param sexp 値
	 * @return リスト
	 */
	public static final ElvaList cast(ElvaNode sexp) {
		if(sexp instanceof ElvaList) return (ElvaList) sexp;
		return new ChainSeq(sexp, NIL);
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
			return value instanceof Object[];
		}

		@Override
		public final ElvaNode encode(Object value) {
			return new ArraySeq((Object[]) value);
		}
	}

	/**
	 * LISP処理系内部で利用される空のリスト構造の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/06
	 */
	public static final class EmptySeq extends ElvaList {
		/**
		 * このコンストラクタは隠蔽されます。
		 */
		private EmptySeq() {}

		/**
		 * 空のリストを返します。
		 *
		 * @return 先頭
		 */
		@Override
		public final ElvaNode head() {
			return this;
		}

		/**
		 * 空のリストを返します。
		 *
		 * @return 後続
		 */
		@Override
		public final BaseList tail() {
			return this;
		}

		/**
		 * 空の部分リストを返します。
		 *
		 * @param skip 部分リストが始まる位置
		 * @return 部分リスト
		 */
		@Override
		public final BaseList drop(int skip) {
			return this;
		}

		/**
		 * 空の部分リストを返します。
		 *
		 * @param size 部分リストが終わる位置
		 * @return 部分リスト
		 */
		@Override
		public final BaseList take(int size) {
			return this;
		}

		/**
		 * 例外を発生します。
		 *
		 * @param index 要素の位置
		 * @return 要素
		 */
		@Override
		public final ElvaNode get(int index) {
			final String msg = String.valueOf(index);
			throw new IndexOutOfBoundsException(msg);
		}

		/**
		 * このリストの要素数を返します。
		 *
		 * @return 要素数
		 */
		@Override
		public final int size() {
			return 0;
		}

		/**
		 * このリストの内容をイテレータで返します。
		 *
		 * @return イテレータ
		 */
		@Override
		public final Iterator<ElvaNode> iterator() {
			return new ChainIt(this);
		}
	}

	/**
	 * LISP処理系内部で利用される配列リスト構造の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/03
	 */
	public static final class ArraySeq extends ElvaList {
		private final Object[] data;
		private final int head;
		private final int tail;

		/**
		 * 内容を指定して配列を構築します。
		 *
		 * @param data 内容
		 */
		public ArraySeq(Object[] data) {
			this(data, 0, data.length);
		}

		/**
		 * 内容と範囲を指定して配列を構築します。
		 *
		 * @param data 内容
		 * @param head 先頭の位置
		 * @param tail 末尾の位置
		 */
		public ArraySeq(Object[] data, int head, int tail) {
			this.data = data;
			this.head = Math.min(head, data.length);
			this.tail = Math.min(tail, data.length);
		}

		/**
		 * このリストの先頭を返します。
		 *
		 * @return 先頭
		 */
		@Override
		public final ElvaNode head() {
			return isEmpty()? NIL: ElvaNode.wrap(data[head]);
		}

		/**
		 * このリストの後続を返します。
		 *
		 * @return 後続
		 */
		@Override
		public final BaseList tail() {
			return new ArraySeq(data, head + 1, tail);
		}

		/**
		 * 指定された位置で始まる部分リストを返します。
		 *
		 * @param skip 部分リストが始まる位置
		 * @return 部分リスト
		 */
		@Override
		public final BaseList drop(int skip) {
			return new ArraySeq(data, head + skip, tail);
		}

		/**
		 * 指定された位置で終わる部分リストを返します。
		 *
		 * @param size 部分リストが終わる位置
		 * @return 部分リスト
		 */
		@Override
		public final BaseList take(int size) {
			return new ArraySeq(data, head, head + size);
		}

		/**
		 * このリストの指定された位置の要素を返します。
		 *
		 * @param index 要素の位置
		 * @return 要素
		 */
		@Override
		public final ElvaNode get(int index) {
			return ElvaNode.wrap(data[head + index]);
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
	}

	/**
	 * LISP処理系内部で利用される連鎖リスト構造の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/18
	 */
	public static final class ChainSeq extends ElvaList {
		private final ElvaNode head;
		private final BaseList tail;
		private final int size;

		/**
		 * 連鎖する要素を指定してリストを構築します。
		 *
		 * @param head 先頭の要素
		 * @param tail 末尾の要素
		 */
		public ChainSeq(ElvaNode head, ElvaList tail) {
			this.head = head;
			this.tail = tail;
			this.size = tail.size() + 1;
		}

		/**
		 * このリストの先頭を返します。
		 *
		 * @return 先頭
		 */
		@Override
		public final ElvaNode head() {
			return head;
		}

		/**
		 * このリストの後続を返します。
		 *
		 * @return 後続
		 */
		@Override
		public final BaseList tail() {
			return tail;
		}

		/**
		 * 指定された位置で始まる部分リストを返します。
		 *
		 * @param skip 部分リストが始まる位置
		 * @return 部分リスト
		 */
		@Override
		public final BaseList drop(int skip) {
			if(skip == 0) return this;
			if(skip >= 1) return tail.drop(skip - 1);
			final String msg = String.valueOf(skip);
			throw new IndexOutOfBoundsException(msg);
		}

		/**
		 * 指定された位置で終わる部分リストを返します。
		 *
		 * @param size 部分リストが終わる位置
		 * @return 部分リスト
		 */
		@Override
		public final BaseList take(int size) {
			return new LimitSeq(this, size);
		}

		/**
		 * このリストの指定された位置の要素を返します。
		 *
		 * @param index 要素の位置
		 * @return 要素
		 */
		@Override
		public final ElvaNode get(int index) {
			if(index < size) return drop(index).head();
			final String msg = String.valueOf(index);
			throw new IndexOutOfBoundsException(msg);
		}

		/**
		 * このリストの要素数を返します。
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
		 * @return イテレータ
		 */
		@Override
		public final Iterator<ElvaNode> iterator() {
			return new ChainIt(this);
		}
	}

	/**
	 * LISP処理系内部で利用される部分リスト構造の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/06
	 */
	public static final class LimitSeq extends ElvaList {
		private final BaseList base;
		private final int size;

		/**
		 * 内容と長さを指定してリストを構築します。
		 *
		 * @param base 内容
		 * @param size 長さ
		 */
		public LimitSeq(BaseList base, int size) {
			this.base = base;
			this.size = size;
		}

		/**
		 * このリストの先頭を返します。
		 *
		 * @return 先頭
		 */
		@Override
		public final ElvaNode head() {
			return size == 0? NIL: base.head();
		}

		/**
		 * このリストの後続を返します。
		 *
		 * @return 後続
		 */
		@Override
		public final BaseList tail() {
			return new LimitSeq(base.tail(), size - 1);
		}

		/**
		 * 指定された位置で始まる部分リストを返します。
		 *
		 * @param skip 部分リストが始まる位置
		 * @return 部分リスト
		 */
		@Override
		public final BaseList drop(int skip) {
			return new LimitSeq(base.drop(skip), size - skip);
		}

		/**
		 * 指定された位置で終わる部分リストを返します。
		 *
		 * @param size 部分リストが終わる位置
		 * @return 部分リスト
		 */
		@Override
		public final BaseList take(int size) {
			return new LimitSeq(this, size);
		}

		/**
		 * このリストの指定された位置の要素を返します。
		 *
		 * @param index 要素の位置
		 * @return 要素
		 */
		@Override
		public final ElvaNode get(int index) {
			if(index < size) return drop(index).head();
			final String msg = String.valueOf(index);
			throw new IndexOutOfBoundsException(msg);
		}

		/**
		 * このリストの要素数を返します。
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
		 * @return イテレータ
		 */
		@Override
		public final Iterator<ElvaNode> iterator() {
			return new ChainIt(this);
		}
	}
}
