/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.JavaCall;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * returns a method of the specified class.
 * <pre>
 * (method 'name class *parameter-types)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/02
 */
@Name("method")
@Args(min = 2, max = -1)
public final class MethodForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var name = eval.apply(args.head());
		final var pars = args.tail().map(eval);
		return new JavaCall(name.name(), pars);
	}
}
