/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.LambdaOp;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * creates and returns an anonymous closure.
 * <pre>
 * (lambda (parameters) body)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/14
 */
@Name("lambda")
@Args(min = 2, max = 2)
public final class LambdaForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new LambdaOp(args, eval);
	}
}
