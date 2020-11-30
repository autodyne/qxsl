/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.StringJoiner;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * LISP処理系内部で利用される複数の値を並べた構造の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/10
 */
public abstract class ListBase extends NodeBase {
	/**
	 * このリストの要素数を返します。
	 *
	 *
	 * @return 要素数
	 */
	public abstract int size();

	/**
	 * このリストの先頭を返します。
	 *
	 *
	 * @return 先頭
	 */
	public abstract NodeBase head();

	/**
	 * このリストの後続を返します。
	 *
	 *
	 * @return 後続
	 */
	public abstract ListBase tail();

	/**
	 * 指定された位置で始まる部分リストを返します。
	 *
	 *
	 * @param skip 部分リストが始まる位置
	 *
	 * @return 部分リスト
	 */
	public abstract ListBase drop(int skip);

	/**
	 * 指定された位置で終わる部分リストを返します。
	 *
	 *
	 * @param size 部分リストが終わる位置
	 *
	 * @return 部分リスト
	 */
	public abstract ListBase take(int size);

	/**
	 * このリストの指定された位置の要素を返します。
	 *
	 *
	 * @param index 要素の位置
	 *
	 * @return 要素
	 */
	public abstract NodeBase get(int index);

	/**
	 * 末尾を除く部分リストを返します。
	 *
	 *
	 * @return 末尾を除去したリスト
	 */
	public final NodeBase init() {
		return take(size() - 1);
	}

	/**
	 * 末尾にある最後の要素を返します。
	 *
	 *
	 * @return 最後の要素
	 */
	public final NodeBase last() {
		return drop(size() - 1).head();
	}

	/**
	 * このリストが空であるか確認します。
	 *
	 *
	 * @return 要素がない場合は真
	 */
	public final boolean isEmpty() {
		return size() == 0;
	}

	/**
	 * この式がアトムであるか確認します。
	 *
	 *
	 * @return 要素なしの場合は真
	 */
	@Override
	public final boolean isAtom() {
		return isEmpty();
	}

	/**
	 * この式を要素のリストに変換します。
	 *
	 *
	 * @return 要素のリスト
	 */
	@Override
	public final List<Object> value() {
		return Arrays.asList(toArray());
	}

	/**
	 * このリストの要素の値を並べた配列を返します。
	 *
	 *
	 * @return 要素の配列
	 */
	public final Object[] toArray() {
		return stream().map(NodeBase::value).toArray();
	}

	/**
	 * このリストに指定された値が含まれるか確認します。
	 *
	 *
	 * @param sexp 確認する値
	 *
	 * @return 含まれる場合に真
	 */
	public final boolean contains(NodeBase sexp) {
		return stream().anyMatch(sexp::equals);
	}

	/**
	 * このリストからハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return value().hashCode();
	}

	/**
	 * このリストと指定された値を比較します。
	 *
	 *
	 * @param sexp 比較対象の値
	 *
	 * @return 等価の場合は真
	 */
	@Override
	public final boolean equals(Object sexp) {
		if(!ListBase.class.isInstance(sexp)) return false;
		final var list = (ListBase) sexp;
		final var lhs = this.iterator();
		final var rhs = list.iterator();
		while(lhs.hasNext() && rhs.hasNext()) {
			if(!lhs.next().equals(rhs.next())) return false;
		}
		return list.size() == size();
	}

	/**
	 * このリストの内容を文字列による表現に変換します。
	 *
	 *
	 * @return 文字列表現
	 */
	@Override
	public final String toString() {
		final var join = new StringJoiner(" ", "(", ")");
		for(var val: this) join.add(String.valueOf(val));
		return join.toString();
	}

	/**
	 * このリストの内容を別のリストに変換します。
	 *
	 *
	 * @param op 写像
	 *
	 * @return 変換されたリスト
	 */
	public ListBase map(UnaryOperator<NodeBase> op) {
		return new ArraySeq(stream().map(op).toArray());
	}

	/**
	 * この引数宣言が可変長引数の関数宣言か確認します。
	 *
	 *
	 * @return 可変長引数なら真
	 */
	public final boolean isVarArgs() {
		return !isEmpty() && last().name().isVarArg();
	}

	/**
	 * 指定された値をリストに変換して返します。
	 * 既にリストである場合はそのまま返します。
	 *
	 *
	 * @param obj 値
	 *
	 * @return リスト
	 */
	public static final ListBase list(Object obj) {
		final NodeBase sexp = NodeBase.wrap(obj);
		if(sexp.isList()) return (ListBase) sexp;
		return new ChainSeq(sexp, NIL);
	}

	/**
	 * 配列リスト構造に適したイテレータの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/03
	 */
	public final class ArrayIt implements Iterator<NodeBase> {
		private ListBase list;
		private int index = 0;

		/**
		 * 指定されたリストの内容を反復します。
		 *
		 *
		 * @param list リスト
		 */
		public ArrayIt(ListBase list) {
			this.list = list;
		}

		/**
		 * 後続の値があるか確認します。
		 *
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
		 *
		 * @return 後続の値
		 */
		@Override
		public NodeBase next() {
			return NodeBase.wrap(list.get(index++));
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
	public final class ChainIt implements Iterator<NodeBase> {
		private ListBase list;
		private final int len;
		private int index = 0;

		/**
		 * 指定されたリストの内容を反復します。
		 *
		 *
		 * @param list リスト
		 */
		public ChainIt(ListBase list) {
			this.len = (this.list = list).size();
		}

		/**
		 * 後続の値があるか確認します。
		 *
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
		 *
		 * @return 後続の値
		 */
		@Override
		public NodeBase next() {
			var value = list.head();
			this.list = list.tail();
			this.index++;
			return value;
		}
	}

	/**
	 * このリストの要素の値を並べた配列を返します。
	 *
	 *
	 * @param <V> 要素の総称型
	 *
	 * @param cls 要素の型
	 *
	 * @return 要素の配列
	 */
	@SuppressWarnings("unchecked")
	public final <V> V[] cast(Class<V> cls) {
		final var array = Array.newInstance(cls, size());
		System.arraycopy(toArray(), 0, array, 0, size());
		return (V[]) array;
	}

	/**
	 * このリストの要素の値を並べた配列を返します。
	 *
	 *
	 * @param cls 要素の型の配列
	 *
	 * @return 要素の配列
	 */
	public final Object[] cast(Class<?>[] cls) {
		int i = 0;
		final var vals = new Object[this.size()];
		for(var v: this) vals[i] = v.to(cls[i++]);
		return vals;
	}

	/**
	 * このリストをストリームに変換します。
	 *
	 *
	 * @return ストリーム
	 */
	public final Stream<NodeBase> stream() {
		return StreamSupport.stream(spliterator(), false);
	}
}
