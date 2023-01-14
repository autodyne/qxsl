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
import elva.warn.ElvaRuntimeException;

/**
 * throws an error with the specified message.
 * <pre>
 * (throw string)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/03
 */
@Name("throw")
@Args(min = 1, max = 1)
public final class ThrowForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var msg = eval.apply(args.head());
		throw new ElvaRuntimeException(msg.text());
	}
}
