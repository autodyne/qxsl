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
import elva.warn.ElvaRuntimeException;

/**
 * throws an error if the expression returns false.
 * <pre>
 * (assert condition message)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/03
 */
@Name("assert")
@Args(min = 2, max = 2)
public final class AssertForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		if(eval.apply(args.head()).bool()) return true;
		final var msg = eval.apply(args.tail().head());
		throw new ElvaRuntimeException(msg.text());
	}
}
