/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.qxsl;

import javax.xml.namespace.QName;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * この関数は指定された名前の修飾名を返します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/03
 */
@Name("name")
@Args(min = 2, max = 2)
public final class NameForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var space = eval.apply(args.get(0)).text();
		final var local = eval.apply(args.get(1)).text();
		return new QName(space, local);
	}
}
