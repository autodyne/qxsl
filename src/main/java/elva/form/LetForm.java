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
 * creates a local variable and evaluates the expression.
 * <pre>
 * (let variable value expression)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/10
 */
@Name("let")
@Args(min = 3, max = -1)
public final class LetForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var arg = args.get(1);
		final var val = eval.apply(arg);
		final var key = args.get(0).name();
		final var env = new ElvaEval(eval);
		env.scope.put(key.toString(), val);
		return args.drop(2).map(env).last();
	}
}