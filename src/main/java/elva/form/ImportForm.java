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
 * imports the specified class.
 * <pre>
 * (import class)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/02
 */
@Name("import")
@Args(min = 1, max = 1)
public final class ImportForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var type = eval.apply(args.head()).type();
		eval.scope.importClass(type.value());
		return type;
	}
}
