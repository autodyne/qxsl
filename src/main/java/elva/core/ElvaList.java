/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * LISP処理系内部で利用されるリスト構造の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public abstract class ElvaList extends ElvaNode implements Iterable<ElvaNode> {
	/**
	 * 内容が空のリストを示す特別なインスタンスです。
	 */
	public static final EmptySeq NIL = new EmptySeq();

	/**
	 * このリストの先頭を返します。
	 *
	 * @return 先頭
	 */
	public abstract ElvaNode head();

	/**
	 * このリストの後続を返します。
	 *
	 * @return 後続
	 */
	public abstract ElvaList tail();

	/**
	 * 指定された位置で始まる部分リストを返します。
	 *
	 * @param skip 部分リストが始まる位置
	 * @return 部分リスト
	 */
	public abstract ElvaList drop(int skip);

	/**
	 * 指定された位置で終わる部分リストを返します。
	 *
	 * @param size 部分リストが終わる位置
	 * @return 部分リスト
	 */
	public abstract ElvaList take(int size);

	/**
	 * このリストの指定された位置の要素を返します。
	 * 範囲を外れると例外が発生する場合があります。
	 *
	 * @param index 要素の位置
	 * @return 要素
	 */
	public abstract ElvaNode get(int index);

	/**
	 * このリストの要素数を返します。
	 *
	 * @return 要素数
	 */
	public abstract int size();

	/**
	 * リストが空であるか確認します。
	 *
	 * @return 要素がない場合は真
	 */
	public final boolean isEmpty() {
		return size() == 0;
	}

	/**
	 * このリストの最後の要素を返します。
	 *
	 * @return 最後の要素
	 */
	public final ElvaNode last() {
		return drop(size() - 1).head();
	}

	/**
	 * このリストのハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Objects.hash(stream().toArray());
	}

	/**
	 * このリストとオブジェクトを比較します。
	 * 同じ内容のリストであれば真を返します。
	 *
	 * @param sexp 比較対象のオブジェクト
	 * @return 同じ内容のリストのみtrue
	 */
	@Override
	public final boolean equals(Object sexp) {
		if(sexp instanceof ElvaList) {
			final var list = (ElvaList) sexp;
			final var s1 = this.stream().toArray();
			final var s2 = list.stream().toArray();
			return Arrays.equals(s1, s2);
		} else return false;
	}

	/**
	 * このリストに指定された値が含まれるか確認します。
	 *
	 * @param sexp 確認する値
	 * @return 含まれる場合にtrue
	 *
	 * @throws NullPointerException sexpがnulである場合
	 */
	public final boolean contains(ElvaNode sexp) {
		for(var v: this) if(sexp.equals(v)) return true;
		return false;
	}

	/**
	 * このリストを実数値の配列に変換します。
	 *
	 * @return 実数値の配列
	 *
	 * @throws ClassCastException 型検査により発生する例外
	 */
	public final List<ElvaReal> reals() {
		final var list = new ArrayList<ElvaReal>(size());
		for(ElvaNode val: this) list.add((ElvaReal) val);
		return list;
	}

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
	 * このリストを指定された型の要素を並べた配列に変換します。
	 *
	 * @param <V> 要素の総称型
	 *
	 * @param type 要素の型
	 * @return 要素の配列
	 */
	@SuppressWarnings("unchecked")
	public final <V extends Object> V[] toArray(Class<V> type) {
		int idx = 0;
		final var seq = (Object[]) Array.newInstance(type, size());
		for(final var value: this) seq[idx++] = value.ofType(type);
		return (V[]) seq;
	}

	/**
	 * このリストの内容をストリームで返します。
	 *
	 * @return ストリーム
	 */
	public final Stream<ElvaNode> stream() {
		return StreamSupport.stream(spliterator(), false);
	}

	/**
	 * このリストの内容を別のリストに変換します。
	 *
	 * @param op 写像
	 * @return 変換されたリスト
	 */
	public final ElvaList map(UnaryOperator<ElvaNode> op) {
		int idx = 0;
		final var vals = new ElvaNode[this.size()];
		for(var v: this) vals[idx++] = op.apply(v);
		return new ArraySeq(vals);
	}

	/**
	 * このリストの内容を文字列による表現に変換します。
	 *
	 * @return 文字列表現
	 */
	@Override
	public final String toString() {
		final var join = new StringJoiner(" ", "(", ")");
		for(ElvaNode val: this) join.add(val.toString());
		return join.toString();
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
		ElvaList chain = NIL;
		final var list = new LinkedList<ElvaNode>();
		for(var e: vals) list.push(ElvaNode.wrap(e));
		for(var e: list) chain = new ChainSeq(e, chain);
		return chain;
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
	 * 指定された配列をリストに変換して返します。
	 *
	 * @param sexp 値
	 * @return リスト
	 *
	 * @throws ClassCastException 配列ではない場合
	 */
	public static final ElvaList asList(Object sexp) {
		return new ArraySeq((Object[]) sexp);
	}

	/**
	 * 指定された値が暗黙的にリストに変換可能か確認します。
	 *
	 * @param sexp 値
	 * @return 配列型の場合は真
	 */
	public static final boolean support(Object sexp) {
		return Object[].class.isInstance(sexp);
	}

	/**
	 * 配列リスト構造に適したイテレータの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/03
	 */
	private final class ArrayIt implements Iterator<ElvaNode> {
		private ElvaList list;
		private int index = 0;

		/**
		 * 指定されたリストの内容を反復します。
		 *
		 * @param list リスト
		 */
		public ArrayIt(ElvaList list) {
			this.list = list;
		}

		/**
		 * 後続の値があるか確認します。
		 *
		 * @return 後続がある場合は真
		 */
		@Override
		public boolean hasNext() {
			return index < list.size();
		}

		/**
		 * 後続の値を取り出します。
		 *
		 * @return 後続の値
		 */
		@Override
		public ElvaNode next() {
			return ElvaNode.wrap(list.get(index++));
		}
	}

	/**
	 * 連鎖リスト構造に適したイテレータの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/02
	 */
	private final class ChainIt implements Iterator<ElvaNode> {
		private ElvaList list;
		private final int len;
		private int index = 0;

		/**
		 * 指定されたリストの内容を反復します。
		 *
		 * @param list リスト
		 */
		public ChainIt(ElvaList list) {
			this.len = (this.list = list).size();
		}

		/**
		 * 後続の値があるか確認します。
		 *
		 * @return 後続がある場合は真
		 */
		@Override
		public boolean hasNext() {
			return index < len;
		}

		/**
		 * 後続の値を取り出します。
		 *
		 * @return 後続の値
		 */
		@Override
		public ElvaNode next() {
			var value = list.head();
			this.list = list.tail();
			this.index++;
			return value;
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
		public final ElvaList tail() {
			return this;
		}

		/**
		 * 空の部分リストを返します。
		 *
		 * @param skip 部分リストが始まる位置
		 * @return 部分リスト
		 */
		@Override
		public final ElvaList drop(int skip) {
			return this;
		}

		/**
		 * 空の部分リストを返します。
		 *
		 * @param size 部分リストが終わる位置
		 * @return 部分リスト
		 */
		@Override
		public final ElvaList take(int size) {
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
			return ElvaNode.wrap(data[head]);
		}

		/**
		 * このリストの後続を返します。
		 *
		 * @return 後続
		 */
		@Override
		public final ElvaList tail() {
			return new ArraySeq(data, head + 1, tail);
		}

		/**
		 * 指定された位置で始まる部分リストを返します。
		 *
		 * @param skip 部分リストが始まる位置
		 * @return 部分リスト
		 */
		@Override
		public final ElvaList drop(int skip) {
			return new ArraySeq(data, head + skip, tail);
		}

		/**
		 * 指定された位置で終わる部分リストを返します。
		 *
		 * @param size 部分リストが終わる位置
		 * @return 部分リスト
		 */
		@Override
		public final ElvaList take(int size) {
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
		private final ElvaList tail;
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
		public final ElvaList tail() {
			return tail;
		}

		/**
		 * 指定された位置で始まる部分リストを返します。
		 *
		 * @param skip 部分リストが始まる位置
		 * @return 部分リスト
		 */
		@Override
		public final ElvaList drop(int skip) {
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
		public final ElvaList take(int size) {
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
		private final ElvaList base;
		private final int size;

		/**
		 * 内容と長さを指定してリストを構築します。
		 *
		 * @param base 内容
		 * @param size 長さ
		 */
		public LimitSeq(ElvaList base, int size) {
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
		public final ElvaList tail() {
			return new LimitSeq(base.tail(), size - 1);
		}

		/**
		 * 指定された位置で始まる部分リストを返します。
		 *
		 * @param skip 部分リストが始まる位置
		 * @return 部分リスト
		 */
		@Override
		public final ElvaList drop(int skip) {
			return new LimitSeq(base.drop(skip), size - skip);
		}

		/**
		 * 指定された位置で終わる部分リストを返します。
		 *
		 * @param size 部分リストが終わる位置
		 * @return 部分リスト
		 */
		@Override
		public final ElvaList take(int size) {
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
