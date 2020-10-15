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

/**
 * performs exclusive-or operation and returns a bool value.
 * <pre>
 * (xor bool1 bool2)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/10
 */
@Name("xor")
@Args(min = 2, max = 2)
public final class XorForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var v1 = eval.apply(args.get(0)).bool();
		final var v2 = eval.apply(args.get(1)).bool();
		return v1 ^ v2;
	}
}
