/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.form;

import java.lang.reflect.Array;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;
import elva.lang.TypeNode;

/**
 * returns an array type.
 * <pre>
 * (array element-type)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/08/29
 */
@Name("array")
@Args(min = 1, max = 1)
public final class ArrayForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final TypeNode type = eval.apply(args.head()).type();
		return Array.newInstance(type.value(), 0).getClass();
	}
}
