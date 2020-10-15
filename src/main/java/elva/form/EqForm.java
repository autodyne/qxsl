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
 * performs identify test and returns a bool value.
 * <pre>
 * (equal value1 value2)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/08/29
 */
@Name("eq")
@Args(min = 2, max = 2)
public final class EqForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var l = eval.apply(args.get(0));
		final var r = eval.apply(args.get(1));
		return l == r;
	}
}
