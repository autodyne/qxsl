/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import java.time.LocalDate;

import elva.lang.ElvaEval;
import elva.lang.FormBase;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.ruler.Contest;

/**
 * creates and returns a contest object.
 * <pre>
 * (contest name host mail link)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
@Name("contest")
@Args(min = 4, max = 4)
public final class ContestForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new ContestImpl(args, eval);
	}
}

/**
 * LISP処理系の内部におけるコンテストの規約の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/20
 */
final class ContestImpl extends Contest {
	private final ElvaEval eval;
	private final String name;
	private final String host;
	private final String mail;
	private final String link;
	private final FormBase from;
	private final FormBase last;
	private final FormBase dead;

	/**
	 * 指定された規約定義と評価器で規約を構築します。
	 *
	 *
	 * @param rule 規約
	 * @param eval 評価器
	 */
	public ContestImpl(ListBase rule, ElvaEval eval) {
		this.name = eval.apply(rule.get(0)).text();
		this.host = eval.apply(rule.get(1)).text();
		this.mail = eval.apply(rule.get(2)).text();
		this.link = eval.apply(rule.get(3)).text();
		this.from = eval.scope.get("start-day").form();
		this.last = eval.scope.get("final-day").form();
		this.dead = eval.scope.get("dead-line").form();
		this.eval = eval;
	}

	/**
	 * コンテストの名前を返します。
	 *
	 *
	 * @return コンテストの名前
	 */
	@Override
	public final String name() {
		return name;
	}

	/**
	 * コンテストの主催者を返します。
	 *
	 *
	 * @return コンテストの主催者
	 */
	@Override
	public final String host() {
		return host;
	}

	/**
	 * コンテストの連絡先を返します。
	 *
	 *
	 * @return コンテストの連絡先
	 */
	@Override
	public final String mail() {
		return mail;
	}

	/**
	 * コンテストの規約の場所を返します。
	 *
	 *
	 * @return コンテストの規約の場所
	 */
	@Override
	public final String link() {
		return link;
	}

	/**
	 * 指定された年のコンテストの開始日を計算します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 開始日
	 */
	@Override
	public final LocalDate getStartDay(int year) {
		return eval.apply(from.form(year)).ofType(LocalDate.class);
	}

	/**
	 * 指定された年のコンテストの終了日を計算します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 終了日
	 */
	@Override
	public final LocalDate getFinalDay(int year) {
		return eval.apply(last.form(year)).ofType(LocalDate.class);
	}

	/**
	 * 指定された年のコンテストの締切日を計算します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 締切日
	 */
	@Override
	public final LocalDate getDeadLine(int year) {
		return eval.apply(dead.form(year)).ofType(LocalDate.class);
	}
}
