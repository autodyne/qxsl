/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * evaluates the expression specified by the key.
 * <pre>
 * (case condition (key value)*)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/10
 */
@Name("case")
@Args(min = 1, max = -1)
public final class CaseForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var k = eval.apply(args.head());
		for(var e: args.tail()) {
			final var lhs = e.list().get(0);
			final var rhs = e.list().get(1);
			final var key = eval.apply(lhs);
			if(k.equals(key)) return eval.apply(rhs);
		}
		return null;
	}
}
