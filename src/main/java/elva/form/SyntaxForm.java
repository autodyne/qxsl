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
import elva.lang.SyntaxOp;

/**
 * 指定された引数と本体で匿名のマクロ式を生成します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/14
 */
@Name("syntax")
@Args(min = 2, max = 2)
public final class SyntaxForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new SyntaxOp(args, eval);
	}
}
