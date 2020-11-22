/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

/**
 * LISP処理系で定義される関数宣言の実体を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/20
 */
public abstract class CustomOp extends FormBase {
	private final ListBase pars;
	private final ElvaEval eval;

	/**
	 * 指定された式と評価器で関数宣言を生成します。
	 *
	 *
	 * @param sexp 定義式
	 * @param eval 評価器
	 */
	public CustomOp(ListBase sexp, ElvaEval eval) {
		this.pars = ListBase.list(sexp.head());
		this.pars.map(NodeBase::name);
		this.eval = eval;
	}

	/**
	 * 指定された引数を登録したスコープを返します。
	 *
	 *
	 * @param args 引数のリスト
	 *
	 * @return スコープ
	 */
	public final ElvaEval localize(ListBase args) {
		final var env = this.eval.scope.fork();
		env.put(this.pars, args);
		return new ElvaEval(env);
	}

	/**
	 * この演算子の実体を返します。
	 *
	 *
	 * @return 演算子の実体
	 */
	@Override
	public final CustomOp value() {
		return this;
	}

	/**
	 * この関数宣言が可変長引数の関数宣言か確認します。
	 *
	 *
	 * @return 可変長引数なら真
	 */
	@Override
	public final boolean isVarArgs() {
		return pars.isVarArgs();
	}

	/**
	 * この関数宣言が取る実引数の最小の個数を返します。
	 *
	 *
	 * @return 最小限の引数の個数
	 */
	@Override
	public final int getMinimumArgumentLength() {
		return pars.size() - (isVarArgs()? 1: 0);
	}

	/**
	 * この関数宣言が取る実引数の最大の個数を返します。
	 *
	 *
	 * @return 最大限の引数の個数
	 */
	@Override
	public final int getMaximumArgumentLength() {
		return isVarArgs()? MAX: pars.size();
	}
}
