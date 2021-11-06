/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.FlexLoad;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * returns the field with the specified name.
 * <pre>
 * (access! 'name)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
@Name("access!")
@Args(min = 1, max = 1)
public final class AccessSpForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new FlexLoad(eval.apply(args.head()).name());
	}
}
