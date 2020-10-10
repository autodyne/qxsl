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
import qxsl.ruler.Failure;

/**
 * この関数は交信記録の検査結果に不可の注釈を付けます。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 */
@Name("failure")
@Args(min = 2, max = 2)
public final class FailureForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var bad = eval.apply(args.get(0));
		final var msg = eval.apply(args.get(1)).value();
		return new Failure(bad.ofType(Item.class), msg);
	}
}
