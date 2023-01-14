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
 * evaluates the argument and print its value.
 * <pre>
 * (print argument)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
@Name("print")
@Args(min = 1, max = 1)
public final class PrintForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var val = eval.apply(args.head());
		System.out.print(val.value());
		System.out.flush();
		return val;
	}
}
