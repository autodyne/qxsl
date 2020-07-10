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
 * LISP処理系の算術演算の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class ArithmeticFunctionProvider {
	/**
	 * 可変長引数の実数値の加算演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/19
	 */
	@ElvaForm.Native("+")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Add extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var reals = args.map(eval).reals();
			final var tails = reals.stream().skip(1);
			return tails.reduce(reals.get(0), ElvaReal::add);
		}
	}

	/**
	 * 可変長引数の実数値の減算演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/19
	 */
	@ElvaForm.Native("-")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Sub extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var reals = args.map(eval).reals();
			final var tails = reals.stream().skip(1);
			return tails.reduce(reals.get(0), ElvaReal::sub);
		}
	}

	/**
	 * 可変長引数の実数値の乗算演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/19
	 */
	@ElvaForm.Native("*")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Mul extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var reals = args.map(eval).reals();
			final var tails = reals.stream().skip(1);
			return tails.reduce(reals.get(0), ElvaReal::mul);
		}
	}

	/**
	 * 可変長引数の実数値の除算演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/19
	 */
	@ElvaForm.Native("/")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Div extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var reals = args.map(eval).reals();
			final var tails = reals.stream().skip(1);
			return tails.reduce(reals.get(0), ElvaReal::div);
		}
	}

	/**
	 * 可変長引数の実数値の剰余演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@ElvaForm.Native("mod")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Mod extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var reals = args.map(eval).reals();
			final var tails = reals.stream().skip(1);
			return tails.reduce(reals.get(0), ElvaReal::mod);
		}
	}
}
