/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.FormBase;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.model.Item;
import qxsl.ruler.Message;
import qxsl.ruler.Section;
import qxsl.ruler.Summary;

/**
 * takes four functions and returns a section object.
 * <pre>
 * (section name code verify unique entity result)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
@Name("section")
@Args(min = 6, max = 6)
public final class SectionForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new SectionImpl(args, eval);
	}
}

/**
 * LISP処理系の内部におけるコンテストの部門の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/20
 */
final class SectionImpl extends Section {
	private final String name;
	private final String code;
	private final ElvaEval eval;
	private final FormBase test;
	private final FormBase call;
	private final FormBase mult;
	private final FormBase calc;

	/**
	 * 指定された部門定義と評価器で部門を構築します。
	 *
	 *
	 * @param rule 部門
	 * @param eval 評価器
	 */
	public SectionImpl(ListBase rule, ElvaEval eval) {
		this.name = eval.apply(rule.get(0)).text();
		this.code = eval.apply(rule.get(1)).text();
		this.test = eval.apply(rule.get(2)).form();
		this.call = eval.apply(rule.get(3)).form();
		this.mult = eval.apply(rule.get(4)).form();
		this.calc = eval.apply(rule.get(5)).form();
		this.eval = eval;
	}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 部門の名前
	 */
	@Override
	public final String name() {
		return name;
	}

	/**
	 * 部門の番号を返します。
	 *
	 *
	 * @return 部門の番号
	 */
	@Override
	public final String code() {
		return code;
	}

	/**
	 * 指定された交信記録の妥当性を検査します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 承認された場合はtrue
	 */
	@Override
	public final Message verify(Item item) {
		return eval.apply(test.form(item)).ofType(Message.class);
	}

	/**
	 * 指定された交信記録の識別子を発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 重複を除くための識別子
	 */
	@Override
	public final Object unique(Item item) {
		return eval.apply(call.form(item)).value();
	}

	/**
	 * 指定された交信記録の総得点を計算します。
	 *
	 *
	 * @param items 交信記録
	 *
	 * @return 総得点
	 *
	 * @since 2020/02/26
	 */
	@Override
	public final int result(Summary items) {
		final var sets = items.toScoreAndEntitySets();
		return eval.apply(calc.form(sets)).real().toInt();
	}

	/**
	 * 指定された交信記録のマルチを発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 総得点を計算する識別子の配列
	 */
	@Override
	public final Object[] entity(Item item) {
		return eval.apply(mult.form(item)).list().toArray();
	}
}
