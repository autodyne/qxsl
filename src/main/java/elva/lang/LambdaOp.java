/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系で定義されるラムダ式の実体を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class LambdaOp extends FormBase {
	private final ListBase pars;
	private final NodeBase body;
	private final ElvaEval lisp;

	/**
	 * 指定された式と評価器でラムダ式を生成します。
	 *
	 *
	 * @param sexp 定義式
	 * @param lisp 評価器
	 */
	public LambdaOp(ListBase sexp, ElvaEval lisp) {
		this.pars = ListBase.list(sexp.get(0));
		this.body = sexp.get(1);
		this.lisp = lisp;
	}

	/**
	 * このラムダ式が可変長引数のラムダ式か確認します。
	 *
	 * @return 可変長引数なら真
	 */
	@Override
	public final boolean isVarArgs() {
		if(pars.isEmpty()) return false;
		return pars.last().toString().endsWith("...");
	}

	/**
	 * このラムダ式が取る引数の最小の個数を返します。
	 *
	 * @return 最小限の引数の個数
	 */
	@Override
	public final int getMinimumArgumentLength() {
		return pars.size() - (isVarArgs()? 1: 0);
	}

	/**
	 * このラムダ式が取る引数の最大の個数を返します。
	 *
	 * @return 最大限の引数の個数
	 */
	@Override
	public final int getMaximumArgumentLength() {
		return isVarArgs()? Integer.MAX_VALUE: pars.size();
	}

	/**
	 * このラムダ式を表す文字列を返します。
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return String.format("(lambda %s %s)", pars, body);
	}

	/**
	 * 指定された実引数と評価器に対し、返り値を求めます。
	 *
	 *
	 * @param args 実引数
	 * @param eval 評価器
	 *
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	@Override
	public final Object apply(ListBase args, ElvaEval eval) {
		final var local = lisp.scope.fork();
		local.put(this.pars, args.map(eval));
		return new ElvaEval(local).apply(body);
	}
}
