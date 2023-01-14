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
import elva.warn.ElvaRuntimeException;

/**
 * evaluates the expression and returns an error.
 * <pre>
 * (catch expression)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/03
 */
@Name("catch")
@Args(min = 1, max = 1)
public final class CatchForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		try {
			eval.apply(args.head());
			return null;
		} catch (ElvaRuntimeException ex) {
			return ex.getError();
		}
	}
}
