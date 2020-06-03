/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;

/**
 * LISP処理系の準引用式の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class QuasiQuoteFunctionProvider {
	/**
	 * 指定された式をそのまま引用する特殊形式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("quote")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Quote extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return args.head();
		}
	}

	/**
	 * 指定された式をそのまま引用する特殊形式です。
	 * 被引用式の内部で引用が部分的に解除されます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("quasiquote")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Quasi extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.unquote(args.head()).sexp();
		}
	}

	/**
	 * 指定された式の引用を解除する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("unquote")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Uquot extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head());
		}
	}

	/**
	 * 指定された式の引用を解除する関数です。
	 * 式の結果はリストの内部に展開されます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@ElvaForm.Native("unquote-splicing")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Uqspl extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head());
		}
	}
}
