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
 * LISP処理系の文字列操作の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class CharactersFunctionProvider {
	/**
	 * LISP処理系で事前に定義されるformat関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@ElvaForm.Native("format")
	@ElvaForm.Parameters(min = 1, max = -1)
	public static final class $Format extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var temp = eval.apply(args.head()).text();
			final var vals = args.tail().map(eval).values();
			return String.format(temp, vals.toArray());
		}
	}
}
