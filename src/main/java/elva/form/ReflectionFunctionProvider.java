/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.core.ElvaType;

/**
 * LISP処理系にメソッドやフィールドを参照する関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class ReflectionFunctionProvider {
	/**
	 * 指定された式を評価した結果の型情報を返す関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/03
	 */
	@ElvaForm.Native("type")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Type extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head()).value().getClass();
		}
	}

	/**
	 * 指定されたクラスと名前のメンバを参照する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/02
	 */
	@ElvaForm.Native("access")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Access extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var type = eval.apply(args.get(0)).type();
			final var name = eval.apply(args.get(1)).name();
			return new Access(new ElvaType(type), name);
		}
	}
}
