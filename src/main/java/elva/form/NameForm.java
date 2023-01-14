/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.form;

import javax.xml.namespace.QName;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * returns a qualified name in the specified namespace.
 * <pre>
 * (name namespace local-name)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/03
 */
@Name("name")
@Args(min = 2, max = 2)
public final class NameForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var space = eval.apply(args.get(0)).text();
		final var local = eval.apply(args.get(1)).text();
		return new QName(space, local);
	}
}
