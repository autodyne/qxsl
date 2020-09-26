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
 * 指定された局所変数を設定して式を評価します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/10
 */
@Name("let")
@Args(min = 3, max = -1)
public final class LetForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var arg = args.get(1);
		final var val = eval.apply(arg);
		final var key = args.get(0).name();
		final var env = new ElvaEval(eval);
		env.scope.put(key.toString(), val);
		return args.drop(2).map(env).last();
	}
}
