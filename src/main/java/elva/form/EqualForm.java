/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.form;

import java.util.Objects;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * performs equality test and returns a bool value.
 * <pre>
 * (equal value1 value2)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("equal")
@Args(min = 2, max = 2)
public final class EqualForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var l = eval.apply(args.get(0));
		final var r = eval.apply(args.get(1));
		return Objects.equals(l, r);
	}
}
