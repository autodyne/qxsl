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
import elva.lang.NodeBase;

/**
 * tests if the argument is nil or not.
 * <pre>
 * (nil? argument)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/10
 */
@Name("nil?")
@Args(min = 1, max = 1)
public final class NilForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return NodeBase.NIL.equals(eval.apply(args.head()));
	}
}
