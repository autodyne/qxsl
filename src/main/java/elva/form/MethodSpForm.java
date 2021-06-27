/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.FlexCall;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * returns a method with the specified name.
 * <pre>
 * (method! 'name [class])
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
@Name("method!")
@Args(min = 1, max = 2)
public final class MethodSpForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var vals = args.map(eval);
		final var name = vals.head().name();
		if(vals.size() < 2) return new FlexCall(name);
		return new FlexCall(name, vals.last().type());
	}
}
