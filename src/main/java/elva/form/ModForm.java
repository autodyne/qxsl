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
import elva.lang.NodeBase;
import elva.lang.RealNode;

/**
 * performs modulo and returns a real value.
 * <pre>
 * (mod real1 *reals)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 */
@Name("mod")
@Args(min = 2, max = -1)
public final class ModForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var seq = args.map(eval).stream();
		final var num = seq.map(NodeBase::real);
		return num.reduce(RealNode::mod).get();
	}
}
