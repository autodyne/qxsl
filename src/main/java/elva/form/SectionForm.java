/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.FormBase;
import elva.lang.ListBase;
import elva.lang.NameNode;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.model.Item;
import qxsl.ruler.Message;
import qxsl.ruler.Section;
import qxsl.ruler.Summary;

/**
 * creates and returns a section object.
 * <pre>
 * (section name code verifier scoring)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
@Name("section")
@Args(min = 4, max = 4)
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
	private final FormBase rule;
	private final FormBase calc;
	private final ElvaEval eval;

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
		this.rule = eval.apply(rule.get(2)).form();
		this.calc = eval.apply(rule.get(3)).form();
		this.eval = eval;
	}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 部門の名前
	 */
	@Override
	public final String getName() {
		return name;
	}

	/**
	 * 部門の番号を返します。
	 *
	 *
	 * @return 部門の番号
	 */
	@Override
	public final String getCode() {
		return code;
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
	public final int total(Summary items) {
		final var args = items.toScoreAndKeys().toArray();
		return eval.apply(calc.form(args)).real().toInt();
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
		return eval.apply(rule.form(item)).ofType(Message.class);
	}

	/**
	 * このコンテスト部門が参照する変数を実行します。
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
	 * このコンテスト部門が参照する関数を実行します。
	 *
	 *
	 * @param name 関数の名前
	 * @param args 関数の引数
	 *
	 * @return 関数の値
	 *
	 * @since 2020/03/09
	 */
	@Override
	public final Object invoke(String name, Object...args) {
		return eval.apply(new NameNode(name).form(args)).value();
	}
}
