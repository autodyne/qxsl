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
 * 変数の可視範囲を設定して複数の式を実行する関数です。
 * この関数の内部で設定された変数は外部に影響しません。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("block")
@Args(min = 1, max = -1)
public final class BlockForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return args.map(new ElvaEval(eval)).last();
	}
}
