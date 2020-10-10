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

import qxsl.ruler.Library;

/**
 * この関数は可視の関数を参照するライブラリを生成します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/26
 */
@Name("library")
@Args(min = 0, max = 0)
public final class LibraryForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new LibraryImpl(args, eval);
	}

	/**
	 * LISP処理系の内部におけるコンテストの規約の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/20
	 */
	public static final class LibraryImpl extends Library {
		private final ElvaEval eval;

		/**
		 * 指定された規約定義と評価器で規約を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public LibraryImpl(ListBase rule, ElvaEval eval) {
			this.eval = eval;
		}

		@Override
		public final Object get(String name) {
			return eval.apply(new NameNode(name));
		}

		@Override
		public final Object invoke(String name, Object...args) {
			return eval.apply(new NameNode(name).form(args)).value();
		}
	}
}
