/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * LISP処理系内部で利用される複数の値を並べた構造の実装です。
 *
 *
 * @param <V> 値の総称型
 *
 * @author 無線部開発班
 *
 * @since 2020/06/10
 */
public interface Sequence<V> extends Iterable<ElvaNode> {
	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 * @return 値
	 */
	public abstract V value();

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
	public default boolean isEmpty() {
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
	public default boolean contains(ElvaNode sexp) {
		return stream().anyMatch(sexp::equals);
	}

	/**
	 * この系列を実数値の配列に変換します。
	 *
	 * @return 実数値の配列
	 *
	 * @throws ClassCastException 型検査により発生する例外
	 */
	public default List<ElvaReal> reals() {
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
	public default <V> V[] toArray(Class<V> type) {
		int i = 0;
		final var seq = Array.newInstance(type, this.size());
		for(var v: this) Array.set(seq, i++, v.ofType(type));
		return (V[]) seq;
	}

	/**
	 * このリストの内容をストリームで返します。
	 *
	 * @return ストリーム
	 */
	public default Stream<ElvaNode> stream() {
		return StreamSupport.stream(spliterator(), false);
	}

	/**
	 * この系列の内容を別の系列に変換します。
	 *
	 * @param op 写像
	 * @return 変換された系列
	 */
	public abstract Sequence map(UnaryOperator<ElvaNode> op);
}
