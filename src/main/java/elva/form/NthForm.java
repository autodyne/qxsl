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
 * returns the n-th element of the list.
 * <pre>
 * (nth index list)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/05
 */
@Name("nth")
@Args(min = 2, max = 2)
public final class NthForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var idx = eval.apply(args.get(0));
		final var seq = eval.apply(args.get(1));
		return seq.list().get(idx.real().toInt());
	}
}
