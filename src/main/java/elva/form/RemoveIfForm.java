/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.form;

import java.util.LinkedList;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;
import elva.lang.NodeBase;

/**
 * removes elements for which the specified function returns false.
 * <pre>
 * (remove-if fun list)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/06/22
 */
@Name("remove-if")
@Args(min = 2, max = 2)
public final class RemoveIfForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var list = new LinkedList<NodeBase>();
		final var test = eval.apply(args.get(0)).form();
		for(final var e: eval.apply(args.get(1)).list()) {
			if(!eval.apply(test.form(e)).bool()) list.add(e);
		}
		return list;
	}
}
