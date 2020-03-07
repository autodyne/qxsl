/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.Serializable;
import elva.Elva.ElvaRuntimeException;

/**
 * LISP処理系で使用されるリストやアトムの抽象化です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/29
 */
public abstract class Sexp implements Serializable {
	/**
	 * リスト自体またはアトムの内容を返します。
	 *
	 * @return 値
	 */
	public abstract Object value();

	/**
	 * この値が識別子であるか確認します。
	 *
	 * @return 識別子の場合にtrue
	 */
	public final boolean isSymbol() {
		return value() instanceof Symbol;
	}

	/**
	 * この値が文字列であるか確認します。
	 *
	 * @return 文字列の場合にtrue
	 */
	public final boolean isString() {
		return value() instanceof String;
	}

	/**
	 * 指定された値を{@link Sexp}で包みます。
	 *
	 * @param sexp 値
	 * @return 値
	 */
	public static final Sexp wrap(Object sexp) {
		if(sexp instanceof Sexp) return (Sexp) sexp;
		return new Atom(sexp);
	}

	/**
	 * この値が指定された型であるか検査します。
	 *
	 * @param type 型
	 * @return 式の値
	 *
	 * @param <V> 返り値の総称型
	 */
	public final <V> V as(Class<V> type) {
		final var value = this.value();
		@SuppressWarnings("unchecked")
		final V valid = type.isInstance(value)? (V) value: null;
		final String temp = "%s instance required but %s found";
		if (valid != null) return valid;
		throw new ElvaRuntimeException(temp, type, value).add(this);
	}
}
