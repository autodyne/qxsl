/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ChainSeq;
import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * 指定された2個の式の値を要素に持つリストを返します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/03
 */
@Name("cons")
@Args(min = 2, max = 2)
public final class ConsForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		final var head = eval.apply(args.get(0));
		final var tail = eval.apply(args.get(1));
		return new ChainSeq(head, tail.list());
	}
}
