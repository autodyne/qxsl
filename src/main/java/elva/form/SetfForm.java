/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import javax.xml.namespace.QName;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.value.Tuple;

/**
 * assigns the specified field value into the item.
 * <pre>
 * (setf item qname value)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/29
 */
@Name("setf")
@Args(min = 3, max = 3)
public final class SetfForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var obj = eval.apply(args.get(0)).ofType(Tuple.class);
		final var qua = eval.apply(args.get(1)).ofType(QName.class);
		final var val = eval.apply(args.get(2)).valueAsString();
		if(val == null || val.isEmpty()) return obj.remove(qua);
		return obj.set(qua, val);
	}
}
