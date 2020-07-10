/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaNode;
import elva.core.BaseList;
import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系の制御構造の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class ProcessingFunctionProvider {
	/**
	 * 条件式を評価して真の場合は左側の式の値を返します。
	 * 偽の場合は右側の式の値または空のリストを返します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@ElvaForm.Native("if")
	@ElvaForm.Parameters(min = 2, max = 3)
	public static final class $If extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var cond = eval.apply(args.head()).bool();
			return eval.apply(args.drop(cond? 1: 2).head());
		}
	}

	/**
	 * 変数の可視範囲を設定して複数の式を実行する関数です。
	 * この関数の内部で設定された変数は外部に影響しません。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@ElvaForm.Native("block")
	@ElvaForm.Parameters(min = 1, max = -1)
	public static final class $Block extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			return args.map(new ElvaEval(eval)).last();
		}
	}

	/**
	 * 指定された文字列で警告を発する例外を発生させます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/03
	 */
	@ElvaForm.Native("throw")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Throw extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final ElvaNode msg = eval.apply(args.head());
			throw new ElvaRuntimeException(msg.text());
		}
	}

	/**
	 * 指定された式で例外が発生した場合は捕捉します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/03
	 */
	@ElvaForm.Native("catch")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Catch extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			try {
				eval.apply(args.head());
				return null;
			} catch (ElvaRuntimeException ex) {
				return ex.getError();
			}
		}
	}

	/**
	 * 指定された条件式が不成立の場合に例外を発生させます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/03
	 */
	@ElvaForm.Native("assert")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Assert extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			if(eval.apply(args.head()).bool()) return true;
			final ElvaNode error = eval.apply(args.get(1));
			throw new ElvaRuntimeException(error.text());
		}
	}
}
