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
 * returns the value of the specified static field.
 * <pre>
 * (static class name)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/03
 */
@Name("static")
@Args(min = 2, max = 2)
public final class StaticForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		try {
			final var cls = eval.apply(args.get(0)).type();
			final var fld = eval.apply(args.get(1)).name();
			return cls.value().getField(fld.toString()).get(null);
		} catch (ReflectiveOperationException ex) {
			throw new ElvaRuntimeException(ex);
		}
	}
}
