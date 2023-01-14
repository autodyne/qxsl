/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.ruler;

import java.util.List;

import elva.lang.ElvaEval;
import elva.lang.FormBase;
import elva.lang.ListBase;
import elva.lang.NameNode;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.local.LocalCityItem;
import qxsl.model.Item;
import qxsl.ruler.Element;
import qxsl.ruler.Message;
import qxsl.ruler.Section;
import qxsl.ruler.Summary;

/**
 * takes four functions and returns a section object.
 * <pre>
 * (section name code cities verify unique entity result)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
@Name("section")
@Args(min = 7, max = 7)
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
	private final ListBase area;
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
		this.area = eval.apply(rule.get(2)).list();
		this.test = eval.apply(rule.get(3)).form();
		this.call = eval.apply(rule.get(4)).form();
		this.mult = eval.apply(rule.get(5)).form();
		this.calc = eval.apply(rule.get(6)).form();
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
	 * @return 部門の名前
	 */
	@Override
	public final String name() {
		return name;
	}

	/**
	 * 部門の分類を返します。
	 *
	 *
	 * @return 部門の分類
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
		final var app = this.test.form(item);
		return eval.apply(app).to(Message.class);
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
	public final Element unique(Item item) {
		final var app = this.call.form(item);
		final var val = this.eval.apply(app);
		return new Element(val.value());
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
		final var data = items.toArray();
		final var form = calc.form(data);
		return eval.apply(form).real().toInt();
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
	public final Element entity(Item item) {
		final var app = this.mult.form(item);
		final var val = this.eval.apply(app);
		return new Element(val.list().toArray());
	}

	/**
	 * この部門に参加可能な運用場所を返します。
	 *
	 *
	 * @return 運用場所
	 *
	 * @since 2022/06/22
	 */
	@Override
	public final List<LocalCityItem> getCityList() {
		return List.of(area.cast(LocalCityItem.class));
	}
}
