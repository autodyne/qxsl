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
import elva.lang.NodeBase;

/**
 * 指定された式を評価して値が空リストか確認する関数です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/10
 */
@Name("nil?")
@Args(min = 1, max = 1)
public final class NilForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		return NodeBase.NIL.equals(eval.apply(args.head()));
	}
}
