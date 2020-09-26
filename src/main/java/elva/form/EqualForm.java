/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import java.util.Objects;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

/**
 * 指定された式を評価して両辺の等値性を確認する関数です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
@Name("equal")
@Args(min = 2, max = 2)
public final class EqualForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var l = eval.apply(args.get(0));
		final var r = eval.apply(args.get(1));
		return Objects.equals(l, r);
	}
}
