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
 * 指定された文字列で警告を発する例外を発生させます。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/03
 */
@Name("throw")
@Args(min = 1, max = 1)
public final class ThrowForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var msg = eval.apply(args.head());
		throw new ElvaRuntimeException(msg.text());
	}
}
