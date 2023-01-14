/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.ruler;

import elva.lang.ElvaEval;
import elva.lang.FormBase;
import elva.lang.ListBase;
import elva.lang.NameNode;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.model.Item;
import qxsl.ruler.Pattern;

/**
 * creates and returns a pattern object.
 * <pre>
 * (pattern normalize transform cross-check)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/26
 */
@Name("pattern")
@Args(min = 3, max = 3)
public final class PatternForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new PatternImpl(args, eval);
	}
}

/**
 * LISP処理系の内部における交信記録の構造定義の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/20
 */
final class PatternImpl extends Pattern {
	private final FormBase norm;
	private final FormBase tran;
	private final FormBase xref;
	private final ElvaEval eval;

	/**
	 * 指定された規約定義と評価器で規約を構築します。
	 *
	 *
	 * @param rule 規約
	 * @param eval 評価器
	 */
	public PatternImpl(ListBase rule, ElvaEval eval) {
		this.norm = eval.apply(rule.get(0)).form();
		this.tran = eval.apply(rule.get(1)).form();
		this.xref = eval.apply(rule.get(2)).form();
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
	 * 相互に交信が成立した無線局間で交信記録を照合します。
	 *
	 *
	 * @param a 片方の無線局の交信記録
	 * @param b 他方の無線局の交信記録
	 *
	 * @return 照合の結果
	 *
	 * @since 2022/08/11
	 */
	@Override
	public final boolean match(Item a, Item b) {
		return eval.apply(xref.form(a, b)).bool();
	}

	/**
	 * 交信記録をライブラリが定義する標準構造に変換します。
	 *
	 *
	 * @param item 交信記録
	 * @param form 変換前の書式 nullを許容する
	 *
	 * @return 標準的な構造の交信記録
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final Item normalize(Item item, String form) {
		return eval.apply(norm.form(item, form)).to(Item.class);
	}

	/**
	 * 交信記録を指定された書式に適合する構造に変換します。
	 *
	 *
	 * @param item 交信記録
	 * @param form 変換後の書式
	 *
	 * @return 書式に適合する交信記録
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final Item transform(Item item, String form) {
		return eval.apply(tran.form(item, form)).to(Item.class);
	}
}
