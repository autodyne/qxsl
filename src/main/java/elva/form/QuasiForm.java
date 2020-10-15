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
 * quotes the expression except for some unquoted sub-expressions.
 * <pre>
 * (quasiquote expression)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/14
 */
@Name("quasiquote")
@Args(min = 1, max = 1)
public final class QuasiForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return eval.quote(args.head()).sexp();
	}
}
