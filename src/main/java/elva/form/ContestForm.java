/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

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
 * (contest name host mail link)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
@Name("contest")
@Args(min = 4, max = 4)
public final class ContestForm extends NativeOp {
	@Override
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
		private final ElvaEval eval;
		private final String name;
		private final String host;
		private final String mail;
		private final String link;

		/**
		 * 指定された規約定義と評価器で規約を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public ContestImpl(ListBase rule, ElvaEval eval) {
			this.name = eval.apply(rule.get(0)).text();
			this.host = eval.apply(rule.get(1)).text();
			this.mail = eval.apply(rule.get(2)).text();
			this.link = eval.apply(rule.get(3)).text();
			this.eval = eval;
		}

		@Override
		public final String getName() {
			return name;
		}

		@Override
		public final String getHost() {
			return host;
		}

		@Override
		public final String getMail() {
			return mail;
		}

		@Override
		public final String getLink() {
			return link;
		}

		@Override
		public final Object get(String name) {
			return eval.apply(new NameNode(name)).value();
		}

		@Override
		public final Object invoke(String name, Object...args) {
			return eval.apply(new NameNode(name).form(args)).value();
		}
	}
}
