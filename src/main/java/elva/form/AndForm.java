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
 * performs and operation and returns a bool value.
 * <pre>
 * (and bool1 *bools)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("and")
@Args(min = 1, max = -1)
public final class AndForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return args.stream().allMatch(s -> eval.apply(s).bool());
	}
}
