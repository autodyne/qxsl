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
 * 指定された式を評価して値が空の参照か確認する関数です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/12/06
 */
@Name("null?")
@Args(min = 1, max = 1)
public final class NullForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		return eval.apply(args.head()).value() == null;
	}
}
