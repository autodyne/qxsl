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
 * tests if the list contains the specified value.
 * <pre>
 * (member value list)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("member")
@Args(min = 2, max = 2)
public final class MemberForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var val = eval.apply(args.get(0));
		final var seq = eval.apply(args.get(1));
		return seq.list().contains(val);
	}
}
