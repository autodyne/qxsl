/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.core.ElvaNode;
import elva.core.BaseList;

/**
 * LISP処理系の関数定義の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class DefinitionFunctionProvider {
	/**
	 * 指定された引数と本体で匿名のラムダ式を生成します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("lambda")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Lambda extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var pars = ElvaList.cast(args.get(0));
			final var body = ElvaNode.wrap(args.get(1));
			return new Lambda(pars, body, eval);
		}
	}

	/**
	 * 指定された引数と本体で匿名のマクロ式を生成します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("syntax")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Syntax extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var pars = ElvaList.cast(args.get(0));
			final var body = ElvaNode.wrap(args.get(1));
			return new Syntax(pars, body, eval);
		}
	}
}
