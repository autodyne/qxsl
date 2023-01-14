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
 * returns a subsequence of the specified list.
 * <pre>
 * (subseq list start end)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/05
 */
@Name("subseq")
@Args(min = 3, max = 3)
public final class SubSeqForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var list = eval.apply(args.get(0));
		final int head = eval.apply(args.get(1)).real().toInt();
		final int tail = eval.apply(args.get(2)).real().toInt();
		return list.list().take(tail).drop(head);
	}
}
