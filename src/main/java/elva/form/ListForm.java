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
 * creates a list of elements as specified.
 * <pre>
 * (list *elements)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/14
 */
@Name("list")
@Args(min = 0, max = -1)
public final class ListForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return args.map(eval);
	}
}
