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
 * 指定された式で例外が発生した場合は捕捉します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/03
 */
@Name("catch")
@Args(min = 1, max = 1)
public final class CatchForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		try {
			eval.apply(args.head());
			return null;
		} catch (ElvaRuntimeException ex) {
			return ex.getError();
		}
	}
}
