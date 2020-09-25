/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.qxsl;

import elva.lang.ElvaEval;
import elva.lang.FormBase;
import elva.lang.ListBase;
import elva.lang.NameNode;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.ruler.Contest;
import qxsl.ruler.Summary;

/**
 * この関数はコンテストの規約の実体を生成します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
@Name("contest")
@Args(min = 2, max = 2)
public final class ContestForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		return new ContestImpl(args, eval);
	}

	/**
	 * LISP処理系の内部におけるコンテストの規約の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/20
	 */
	public static final class ContestImpl extends Contest {
		private final String name;
		private final FormBase rule;
		private final ElvaEval eval;

		/**
		 * 指定された規約定義と評価器で規約を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public ContestImpl(ListBase rule, ElvaEval eval) {
			this.name = eval.apply(rule.get(0)).text();
			this.rule = eval.apply(rule.get(1)).form();
			this.eval = eval;
		}

		@Override
		public final String getName() {
			return name;
		}

		@Override
		public final int score(Summary items) {
			if(items.score() == 0) return 0;
			final var args = items.toScoreAndKeys().toArray();
			return eval.apply(rule.form(args)).real().toInt();
		}

		@Override
		public final Object invoke(String name, Object...args) {
			return eval.apply(new NameNode(name).form(args)).value();
		}
	}
}
