/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import java.lang.reflect.Array;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;
import elva.lang.NativeOp;
import elva.lang.TypeNode;

/**
 * 指定された型の要素を持つ配列型を返します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/08/29
 */
@Name("array")
@Args(min = 1, max = 1)
public final class ArrayForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		final TypeNode type = eval.apply(args.head()).type();
		return Array.newInstance(type.value(), 0).getClass();
	}
}
