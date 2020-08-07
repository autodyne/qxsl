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
 * 指定された引数と書式で文字列を生成します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/26
 */
@Name("format")
@Args(min = 1, max = -1)
public final class FormatForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		final var list = args.map(eval);
		final var temp = list.head().text();
		final var vals = list.tail().toArray();
		return String.format(temp, vals);
	}
}
