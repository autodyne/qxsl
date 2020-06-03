/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import java.util.StringJoiner;

/**
 * LISP処理系で使用されるリストやアトムの抽象化です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/29
 */
public abstract class ElvaNode implements Serializable {
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
		final var num = ofClass(Number.class);
		return new ElvaReal(num).toBigDecimal();
	}

	/**
	 * この式の真偽型の内容を返します。
	 *
	 * @return 真偽
	 */
	public final boolean bool() {
		return ofClass(Boolean.class);
	}

	/**
	 * この式の識別子型の内容を返します。
	 *
	 * @return 識別子
	 */
	public final ElvaName name() {
		return ofClass(ElvaName.class);
	}

	/**
	 * この式の型情報型の内容を返します。
	 *
	 * @return 型情報
	 */
	public final Class<?> type() {
		return ofClass(Class.class);
	}

	/**
	 * この式の文字列型の内容を返します。
	 *
	 * @return 文字列
	 */
	public final String text() {
		return ofClass(String.class);
	}

	/**
	 * この式のリスト型の内容を返します。
	 *
	 * @return リスト
	 */
	public final ElvaList list() {
		return ofClass(ElvaList.class);
	}

	/**
	 * この式の演算子型の内容を返します。
	 *
	 * @return 演算子
	 */
	public final ElvaForm form() {
		return ofClass(ElvaForm.class);
	}

	/**
	 * この式の指定された型における内容を返します。
	 *
	 * @param type 型
	 * @return 式の値
	 *
	 * @param <V> 返り値の総称型
	 */
	public final <V> V ofClass(Class<V> type) {
		final var body = value();
		final var self = body.getClass();
		if(type.isAssignableFrom(self)) return type.cast(body);
		final var temp = "%s (%s) is detected but %s required";
		final var text = String.format(temp, body, self, type);
		throw new ClassCastException(text);
	}

	/**
	 * この式が指定された型に適合するか確認します。
	 *
	 * @param type 型
	 * @return 式の値
	 */
	public Object isClass(Class<?> type) {
		return ofClass(type);
	}

	/**
	 * 指定された値を{@link ElvaNode}で包みます。
	 *
	 * @param sexp 値
	 * @return 値
	 */
	public static final ElvaNode wrap(Object sexp) {
		if(sexp instanceof ElvaNode) return (ElvaNode) sexp;
		if(ElvaType.support(sexp)) return ElvaType.asType(sexp);
		if(ElvaText.support(sexp)) return ElvaText.asText(sexp);
		if(ElvaReal.support(sexp)) return ElvaReal.asReal(sexp);
		if(ElvaBool.support(sexp)) return ElvaBool.asBool(sexp);
		if(ElvaList.support(sexp)) return ElvaList.asList(sexp);
		return new ElvaWrap(sexp);
	}
}
