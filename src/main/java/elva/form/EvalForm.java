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
 * evaluates the specified value.
 * <pre>
 * (eval value)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/26
 */
@Name("eval")
@Args(min = 1, max = 1)
public final class EvalForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return eval.apply(eval.apply(args.head()));
	}
}
