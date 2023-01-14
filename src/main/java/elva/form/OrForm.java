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
 * performs or operation and returns a bool value.
 * <pre>
 * (or bool1 *bools)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("or")
@Args(min = 1, max = -1)
public final class OrForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return args.stream().anyMatch(s -> eval.apply(s).bool());
	}
}
