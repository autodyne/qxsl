/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * 条件式を評価して真の場合は左側の式の値を返します。
 * 偽の場合は右側の式の値または空のリストを返します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("if")
@Args(min = 2, max = 3)
public final class IfForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		final var cond = eval.apply(args.head()).bool();
		return eval.apply(args.drop(cond? 1: 2).head());
	}
}
