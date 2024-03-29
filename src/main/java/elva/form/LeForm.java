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
 * performs less-equal operation and returns a bool value.
 * <pre>
 * (&lt;= real1 *reals)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/17
 */
@Name("<=")
@Args(min = 2, max = -1)
public final class LeForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var list = args.map(eval);
		for(int i = 1; i < list.size(); i++) {
			final var l = list.get(i - 1).real();
			final var r = list.get(i + 0).real();
			if(l.compareTo(r) > 0) return false;
		}
		return true;
	}
}
