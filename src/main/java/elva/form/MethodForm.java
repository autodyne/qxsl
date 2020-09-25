/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.CreateOp;
import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.MethodOp;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * 指定されたクラスと名前と引数型のメソッドを取得します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/02
 */
@Name("method")
@Args(min = 2, max = -1)
public final class MethodForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		final var name = eval.apply(args.head()).name();
		if(name.toString().equals("new")) {
			return new CreateOp(name, args.map(eval).tail());
		} else {
			return new MethodOp(name, args.map(eval).tail());
		}
	}
}
