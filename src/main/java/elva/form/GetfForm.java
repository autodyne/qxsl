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
 * extracts the specified field value of the item.
 * <pre>
 * (getf item qname)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/29
 */
@Name("getf")
@Args(min = 2, max = 2)
public final class GetfForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var obj = eval.apply(args.get(0)).ofType(Tuple.class);
		final var qua = eval.apply(args.get(1)).ofType(QName.class);
		return obj.value(qua);
	}
}
