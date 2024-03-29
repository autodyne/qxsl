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
import elva.lang.SyntaxOp;

/**
 * creates and returns an anonymous macro.
 * <pre>
 * (syntax parameter-names body)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/14
 */
@Name("syntax")
@Args(min = 2, max = 2)
public final class SyntaxForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new SyntaxOp(args, eval);
	}
}
