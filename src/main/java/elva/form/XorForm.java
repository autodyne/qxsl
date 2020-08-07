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
 * 指定された式を評価して排他的論理和を計算します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/10
 */
@Name("xor")
@Args(min = 2, max = 2)
public final class XorForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		final var v1 = eval.apply(args.get(0)).bool();
		final var v2 = eval.apply(args.get(1)).bool();
		return v1 ^ v2;
	}
}
