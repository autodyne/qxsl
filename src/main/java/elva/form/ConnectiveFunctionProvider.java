/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaReal;
import elva.core.BaseList;

/**
 * LISP処理系の論理演算の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class ConnectiveFunctionProvider {
	/**
	 * 指定された式を短絡評価して論理積を計算します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@ElvaForm.Native("and")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $And extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			return args.stream().allMatch(s -> eval.apply(s).bool());
		}
	}

	/**
	 * 指定された式を短絡評価して論理和を計算します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@ElvaForm.Native("or")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Or extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			return args.stream().anyMatch(s -> eval.apply(s).bool());
		}
	}

	/**
	 * 指定された式を評価して排他的論理和を計算します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@ElvaForm.Native("xor")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Xor extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var v1 = eval.apply(args.get(0)).bool();
			final var v2 = eval.apply(args.get(1)).bool();
			return v1 ^ v2;
		}
	}

	/**
	 * 指定された式を評価して論理的否定を計算します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@ElvaForm.Native("not")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Not extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			return !eval.apply(args.head()).bool();
		}
	}
}
