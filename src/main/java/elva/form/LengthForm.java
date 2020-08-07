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
 * 指定された式を評価してリストの長さを返します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/14
 */
@Name("length")
@Args(min = 1, max = 1)
public final class LengthForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		return eval.apply(args.head()).list().size();
	}
}
