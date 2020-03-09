/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import java.util.StringJoiner;

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
	 * この式の内容を返します。
	 *
	 * @return 値
	 */
	public abstract Object value();

	/**
	 * この式の整数型の内容を返します。
	 *
	 * @return 整数
	 */
	public final int ival() {
		return real().intValueExact();
	}

	/**
	 * この式の実数型の内容を返します。
	 *
	 * @return 実数
	 */
	public final BigDecimal real() {
		return value(BigDecimal.class);
	}

	/**
	 * この式の真偽型の内容を返します。
	 *
	 * @return 真偽
	 */
	public final boolean bool() {
		return value(Boolean.class);
	}

	/**
	 * この式の識別子型の内容を返します。
	 *
	 * @return 識別子
	 */
	public final Name name() {
		return value(Name.class);
	}

	/**
	 * この式の文字列型の内容を返します。
	 *
	 * @return 文字列
	 */
	public final String text() {
		return value(String.class);
	}

	/**
	 * この式のリスト型の内容を返します。
	 *
	 * @return リスト
	 */
	public final Cons cons() {
		return value(Cons.class);
	}

	/**
	 * この式の演算子型の内容を返します。
	 *
	 * @return 演算子
	 */
	public final Form form() {
		return value(Form.class);
	}

	/**
	 * この式の指定された型における内容を返します。
	 *
	 * @param type 型
	 * @return 式の値
	 *
	 * @param <V> 返り値の総称型
	 */
	public final <V> V value(Class<V> type) {
		final var body = value();
		if(type.isInstance(body)) return type.cast(body);
		final var temp = "type %s required but %s found";
		throw new ElvaRuntimeException(temp, type, this);
	}

	/**
	 * 指定された値を{@link Sexp}で包みます。
	 *
	 * @param sexp 値
	 * @return 値
	 */
	public static final Sexp wrap(Object sexp) {
		if(sexp instanceof Sexp) return (Sexp) sexp;
		if(sexp instanceof String) return new Text((String) sexp);
		if(sexp instanceof Number) return new Real((Number) sexp);
		return new Atom(sexp);
	}
}
