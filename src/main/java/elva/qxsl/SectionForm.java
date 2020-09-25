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

import qxsl.model.Item;
import qxsl.ruler.Message;
import qxsl.ruler.Section;

/**
 * この関数はコンテストの部門の実体を生成します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
@Name("section")
@Args(min = 2, max = 3)
public final class SectionForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		return new SectionImpl(args, eval);
	}

	/**
	 * LISP処理系の内部におけるコンテストの部門の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/20
	 */
	public static final class SectionImpl extends Section {
		private final String name;
		private final String code;
		private final FormBase rule;
		private final ElvaEval eval;

		/**
		 * 指定された部門定義と評価器で部門を構築します。
		 *
		 * @param rule 部門
		 * @param eval 評価器
		 */
		public SectionImpl(ListBase rule, ElvaEval eval) {
			this.name = eval.apply(rule.get(0)).text();
			this.code = eval.apply(rule.get(1)).text();
			this.rule = eval.apply(rule.get(2)).form();
			this.eval = eval;
		}

		@Override
		public final String getName() {
			return name;
		}

		@Override
		public final String getCode() {
			return code;
		}

		@Override
		public final Message apply(Item item) {
			return eval.apply(rule.form(item)).ofType(Message.class);
		}

		@Override
		public final Object invoke(String name, Object...args) {
			return eval.apply(new NameNode(name).form(args)).value();
		}
	}
}
