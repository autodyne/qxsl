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
public final class LambdaOp extends CustomOp {
	private final NodeBase body;
	private final ListBase sexp;

	/**
	 * 指定された式と評価器でラムダ式を生成します。
	 *
	 *
	 * @param sexp 定義式
	 * @param eval 評価器
	 */
	public LambdaOp(ListBase sexp, ElvaEval eval) {
		super(sexp, eval);
		this.sexp = sexp;
		this.body = sexp.last();
	}

	/**
	 * このラムダ式を表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return new NameNode("lambda").form(sexp).toString();
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
		return new ElvaEval(localize(args.map(eval))).apply(body);
	}
}
