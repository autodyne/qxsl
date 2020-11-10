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

import qxsl.model.Item;
import qxsl.ruler.Success;

/**
 * creates and returns a success object.
 * <pre>
 * (success item score)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 */
@Name("success")
@Args(min = 2, max = 2)
public final class SuccessForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var obj = eval.apply(args.get(0));
		final var run = eval.apply(args.get(1));
		final var ref = obj.to(Item.class);
		return new Success(ref, run.real().toInt());
	}
}
