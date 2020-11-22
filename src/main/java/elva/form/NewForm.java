/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.JavaInit;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * returns a constructor of the specified class.
 * <pre>
 * (new class *parameter-types)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
@Name("new")
@Args(min = 1, max = -1)
public final class NewForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var type = eval.apply(args.head());
		final var pars = args.tail().map(eval);
		return new JavaInit(type.type(), pars);
	}
}
