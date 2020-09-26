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
import elva.lang.RealNode;

/**
 * 可変長引数の実数値の剰余演算子です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 */
@Name("mod")
@Args(min = 2, max = -1)
public final class ModForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var reals = args.map(eval).reals();
		final var tails = reals.stream().skip(1);
		return tails.reduce(reals.get(0), RealNode::mod);
	}
}
