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
 * 左側の値が右側の値未満であるか確認する関数です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/17
 */
@Name("<")
@Args(min = 2, max = -1)
public final class LtForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var list = args.map(eval);
		for(int i = 1; i < list.size(); i++) {
			final var l = list.get(i - 1).real();
			final var r = list.get(i + 0).real();
			if(l.compareTo(r) >= 0) return false;
		}
		return true;
	}
}
