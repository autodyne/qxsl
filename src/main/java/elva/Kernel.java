/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import java.math.BigDecimal;

/**
 * LISP処理系のスコープ付きの評価器の実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/18
 */
public final class Kernel {
	/**
	 * この評価器に関連づけられたスコープです。
	 *
	 * @return スコープ
	 */
	public final Nested scope;

	/**
	 * 指定されたスコープに対する評価器を構築します。
	 *
	 * @param scope 評価器のスコープ
	 */
	public Kernel(Nested scope) {
		this.scope = scope;
	}

	/**
	 * 指定された式の値を求めて{@link Symbol}として返します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public Symbol name(Object sexp) {
		return eval(sexp, Symbol.class);
	}

	/**
	 * 指定された式の値を求めて{@link Struct}として返します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public Struct list(Object sexp) {
		return eval(sexp, Struct.class);
	}

	/**
	 * 指定された式の値を求めて真偽値として返します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public boolean bool(Object sexp) {
		return eval(sexp, Boolean.class);
	}

	/**
	 * 指定された式の値を求めて実数値として返します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public BigDecimal real(Object sexp) {
		return eval(sexp, BigDecimal.class);
	}

	/**
	 * 指定された式の値を求めて文字列として返します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public String text(Object sexp) {
		return eval(sexp, String.class);
	}

	/**
	 * 指定された式の値を求めます。
	 *
	 * @param sexp 式
	 * @return 返り値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public Object eval(Object sexp) {
		if(Struct.NIL.equals(sexp)) return Struct.NIL;
		if(sexp instanceof Symbol) return scope.get(sexp);
		if(sexp instanceof Struct) return call((Struct) sexp);
		return sexp;
	}

	/**
	 * 指定された式の値を求め、値が指定された型であるか検査します。
	 *
	 * @param sexp 式
	 * @param type 型
	 * @return 返り値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public <V> V eval(Object sexp, Class<V> type) {
		final Object value = eval(sexp);
		@SuppressWarnings("unchecked")
		final V valid = type.isInstance(value)? (V) value: null;
		final String temp = "%s instance required but %s found";
		if (valid != null) return valid;
		throw new ElvaRuntimeException(temp, type, sexp);
	}

	/**
	 * 指定された式を関数適用として評価した値を返します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Object call(Struct sexp) {
		final Function func = eval(sexp.car(), Function.class);
		return valid(func, sexp.cdr()).apply(sexp.cdr(), this);
	}

	/**
	 * 実引数の個数を検査して必要なら例外を発生させます。
	 *
	 * @param func 演算子
	 * @param list 引数の式
	 * @throws ElvaRuntimeException 引数の個数が誤っている場合
	 */
	private final Function valid(Function func, Struct list) {
		String temp = "%s requires at-least %d and at-most %d arguments";
		final Params annon = func.getClass().getAnnotation(Params.class);
		final int len = list.size();
		final int min = annon.min() >= 0? annon.min(): Integer.MAX_VALUE;
		final int max = annon.max() >= 0? annon.max(): Integer.MAX_VALUE;
		if (min <= len && len <= max) return func;
		throw new ElvaRuntimeException(temp, func, min, max);
	}
}
