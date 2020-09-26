/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.qxsl;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.model.Item;

/**
 * この関数は交信記録の項目を生成します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/26
 */
@Name("item")
@Args(min = 0, max = 0)
public final class ItemForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new Item();
	}
}
