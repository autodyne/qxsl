/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.qxsl;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NameNode;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.ruler.Contest;

/**
 * creates and returns a contest object.
 * <pre>
 * (contest name)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
@Name("contest")
@Args(min = 1, max = 1)
public final class ContestForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new ContestImpl(args, eval);
	}
}

/**
 * LISP処理系の内部におけるコンテストの規約の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/20
 */
final class ContestImpl extends Contest {
	private final ElvaEval eval;
	private final String name;

	/**
	 * 指定された規約定義と評価器で規約を構築します。
	 *
	 *
	 * @param rule 規約
	 * @param eval 評価器
	 */
	public ContestImpl(ListBase rule, ElvaEval eval) {
		this.name = eval.apply(rule.head()).text();
		this.eval = eval;
	}

	/**
	 * 規約が参照する変数値を返します。
	 *
	 *
	 * @param name 変数の名前
	 *
	 * @return 変数の値
	 *
	 * @since 2020/09/27
	 */
	@Override
	public final Object get(String name) {
		return eval.apply(new NameNode(name)).value();
	}

	/**
	 * コンテストの名前を返します。
	 *
	 *
	 * @return コンテストの名前
	 */
	@Override
	public final String name() {
		return name;
	}
}
