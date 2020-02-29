/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import elva.Elva.ElvaRuntimeException;

/**
 * LISP処理系で使用されるリストやアトムの抽象化です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2020/02/29
 */
public interface Sexp extends java.io.Serializable {
	/**
	 * リスト自体またはアトムの内容を返します。
	 *
	 * @return 値
	 */
	public Object value();

	/**
	 * 指定された値を{@link Sexp}で包みます。
	 *
	 * @param sexp 値
	 * @return 値
	 */
	static Sexp wrap(Object sexp) {
		if (sexp instanceof Sexp) return (Sexp) sexp;
		return new Atom(sexp);
	}

	/**
	 * この値が識別子であるか確認します。
	 *
	 * @return 識別子の場合にtrue
	 */
	default boolean isSymbol() {
		return value() instanceof Symbol;
	}

	/**
	 * この値が文字列であるか確認します。
	 *
	 * @return 文字列の場合にtrue
	 */
	default boolean isString() {
		return value() instanceof String;
	}

	/**
	 * この値を指定された型のアトムとして返します。
	 *
	 * @return アトム
	 * @throws ElvaRuntimeException 型検査の例外
	 */
	public default Atom atom() throws ElvaRuntimeException {
		try {
			return (Atom) this;
		} catch(ClassCastException ex) {
			final String msg = "lisp expression %s is non-atom";
			throw new ElvaRuntimeException(msg, this).add(this);
		}
	}

	/**
	 * この値をリストとして返します。
	 *
	 * @return リスト
	 * @throws ElvaRuntimeException 型検査の例外
	 */
	public default Cons cons() throws ElvaRuntimeException {
		try {
			return (Cons) this;
		} catch(ClassCastException ex) {
			final String msg = "lisp expression %s is non-cons";
			throw new ElvaRuntimeException(msg, this).add(this);
		}
	}

	/**
	 * この値が指定された型であるか検査します。
	 *
	 * @param type 型
	 * @return 式の値
	 *
	 * @param <V> 返り値の総称型
	 *
	 * @throws ElvaRuntimeException 型検査の例外
	 */
	public default <V> V as(Class<V> type) throws ElvaRuntimeException {
		final var value = this.value();
		@SuppressWarnings("unchecked")
		final V valid = type.isInstance(value)? (V) value: null;
		final String temp = "%s instance required but %s found";
		if (valid != null) return valid;
		throw new ElvaRuntimeException(temp, type, value).add(this);
	}
}
