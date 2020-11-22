/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;
import elva.lang.RealNode;

/**
 * performs addition and returns a real value.
 * <pre>
 * (+ real1 *reals)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/19
 */
@Name("+")
@Args(min = 2, max = -1)
public final class AddForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var reals = args.map(eval).reals();
		final var tails = reals.stream().skip(1);
		return tails.reduce(reals.get(0), RealNode::add);
	}
}
