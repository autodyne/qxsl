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
 * creates a variable with the specified name and value.
 * <pre>
 * (set name value)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("set")
@Args(min = 2, max = 2)
public final class SetForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var key = eval.apply(args.get(0));
		final var val = eval.apply(args.get(1));
		eval.scope.put(key.name().toString(), val);
		return val;
	}
}
