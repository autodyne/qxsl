/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.FlexInit;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * returns a constructor of the specified class.
 * <pre>
 * (new! class)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
@Name("new!")
@Args(min = 1, max = 1)
public final class NewSpForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new FlexInit(eval.apply(args.head()).type());
	}
}
