/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NameNode;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.ruler.Absence;

/**
 * creates and returns a absence object.
 * <pre>
 * (absence name code)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/23
 */
@Name("absence")
@Args(min = 2, max = 2)
public final class AbsenceForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new AbsenceImpl(args, eval);
	}
}

/**
 * LISP処理系の内部における不参加の部門の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/23
 */
final class AbsenceImpl extends Absence {
	private final String name;
	private final String code;
	private final ElvaEval eval;

	/**
	 * 指定された部門定義と評価器で部門を構築します。
	 *
	 *
	 * @param rule 部門
	 * @param eval 評価器
	 */
	public AbsenceImpl(ListBase rule, ElvaEval eval) {
		this.name = eval.apply(rule.head()).text();
		this.code = eval.apply(rule.last()).text();
		this.eval = eval;
	}

	/**
	 * 規約が参照する変数値を返します。
	 *
	 *
	 * @param name 変数の名前
	 *
	 * @return 変数の値
	 *
	 * @since 2020/09/27
	 */
	@Override
	public final Object get(String name) {
		return eval.apply(new NameNode(name)).value();
	}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 名前
	 */
	@Override
	public final String name() {
		return name;
	}

	/**
	 * 部門の番号を返します。
	 *
	 *
	 * @return 番号
	 */
	@Override
	public final String code() {
		return code;
	}
}
