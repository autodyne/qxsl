/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.JavaLoad;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * returns the field of the specified class.
 * <pre>
 * (access 'name class)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/03
 */
@Name("access")
@Args(min = 2, max = 2)
public final class AccessForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var name = eval.apply(args.head()).name();
		final var type = eval.apply(args.last()).type();
		return new JavaLoad(name, type);
	}
}
