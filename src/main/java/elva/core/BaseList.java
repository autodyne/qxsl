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
import java.util.List;
import java.util.Objects;
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
public abstract class BaseList extends ElvaNode implements Iterable<ElvaNode> {
	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 * @return 値
	 */
	public abstract Object value();

	/**
	 * この系列の先頭を返します。
	 *
	 * @return 先頭
	 */
	public abstract ElvaNode head();

	/**
	 * この系列の後続を返します。
	 *
	 * @return 後続
	 */
	public abstract BaseList tail();

	/**
	 * 指定された位置で始まる部分系列を返します。
	 *
	 * @param skip 部分系列が始まる位置
	 * @return 部分系列
	 */
	public abstract BaseList drop(int skip);

	/**
	 * 指定された位置で終わる部分系列を返します。
	 *
	 * @param size 部分系列が終わる位置
	 * @return 部分系列
	 */
	public abstract BaseList take(int size);

	/**
	 * この系列の指定された位置の要素を返します。
	 * 範囲を外れると例外が発生する場合があります。
	 *
	 * @param index 要素の位置
	 * @return 要素
	 */
	public abstract ElvaNode get(int index);

	/**
	 * このリストの最後の要素を返します。
	 *
	 * @return 最後の要素
	 */
	public final ElvaNode last() {
		return drop(size() - 1).head();
	}

	/**
	 * この系列の要素数を返します。
	 *
	 * @return 要素数
	 */
	public abstract int size();

	/**
	 * 系列が空であるか確認します。
	 *
	 * @return 要素がない場合は真
	 */
	public final boolean isEmpty() {
		return size() == 0;
	}

	/**
	 * この系列に指定された値が含まれるか確認します。
	 *
	 * @param sexp 確認する値
	 * @return 含まれる場合にtrue
	 *
	 * @throws NullPointerException 引数がnulである場合
	 */
	public final boolean contains(ElvaNode sexp) {
		return stream().anyMatch(sexp::equals);
	}

	/**
	 * この系列を実数値の配列に変換します。
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
	 * この系列を指定された型の要素を並べた配列に変換します。
	 *
	 * @param <V> 要素の総称型
	 *
	 * @param type 要素の型
	 * @return 要素の配列
	 */
	@SuppressWarnings("unchecked")
	public final <V> V[] toArray(Class<V> type) {
		int i = 0;
		final var seq = Array.newInstance(type, this.size());
		for(var v: this) Array.set(seq, i++, v.ofType(type));
		return (V[]) seq;
	}

	/**
	 * この系列の内容をストリームで返します。
	 *
	 * @return ストリーム
	 */
	public final Stream<ElvaNode> stream() {
		return StreamSupport.stream(spliterator(), false);
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
		if(sexp instanceof BaseList) {
			final var list = (BaseList) sexp;
			final var s1 = this.stream().toArray();
			final var s2 = list.stream().toArray();
			return Arrays.equals(s1, s2);
		} else return false;
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
	 * この系列の内容を別の系列に変換します。
	 *
	 * @param op 写像
	 * @return 変換された系列
	 */
	public abstract BaseList map(UnaryOperator<ElvaNode> op);

	/**
	 * 配列リスト構造に適したイテレータの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/03
	 */
	final class ArrayIt implements Iterator<ElvaNode> {
		private BaseList list;
		private int index = 0;

		/**
		 * 指定されたリストの内容を反復します。
		 *
		 * @param list リスト
		 */
		public ArrayIt(BaseList list) {
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
	final class ChainIt implements Iterator<ElvaNode> {
		private BaseList list;
		private final int len;
		private int index = 0;

		/**
		 * 指定されたリストの内容を反復します。
		 *
		 * @param list リスト
		 */
		public ChainIt(BaseList list) {
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
}
