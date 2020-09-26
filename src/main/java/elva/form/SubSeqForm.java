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
 * 指定された式を評価してリストの指定位置の部分を返します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/05
 */
@Name("subseq")
@Args(min = 3, max = 3)
public final class SubSeqForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var list = eval.apply(args.get(0));
		final int head = eval.apply(args.get(1)).real().toInt();
		final int tail = eval.apply(args.get(2)).real().toInt();
		return list.list().take(tail).drop(head);
	}
}
