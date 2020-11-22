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
 * performs or operation and returns a bool value.
 * <pre>
 * (or bool1 *bools)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("or")
@Args(min = 1, max = -1)
public final class OrForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return args.stream().anyMatch(s -> eval.apply(s).bool());
	}
}
