/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.ruler;

import java.util.ArrayList;
import java.util.Arrays;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.core.ElvaName;
import elva.core.ElvaNode;
import elva.core.ElvaReal;
import elva.core.BaseList;

import qxsl.ruler.Contest;
import qxsl.ruler.Section;
import qxsl.ruler.Summary;

/**
 * LISP処理系の内部におけるコンテストの規約の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/20
 */
public final class ElvaContest extends Contest {
	private final String name;
	private final ElvaForm rule;
	private final ElvaEval eval;

	/**
	 * 指定された規約定義と評価器で規約を構築します。
	 *
	 * @param rule 規約
	 * @param eval 評価器
	 */
	public ElvaContest(BaseList rule, ElvaEval eval) {
		this.name = eval.apply(rule.get(0)).text();
		this.rule = eval.apply(rule.get(1)).form();
		this.eval = eval;
	}

	@Override
	public final String getName() {
		return this.name;
	}

	@Override
	public final int score(final Summary summ) {
		if(summ.score() == 0) return 0;
		final var head = Arrays.asList(summ.score());
		final var list = new ArrayList<Object>(head);
		for(var m: summ.mults()) list.add(ElvaList.array(m));
		return eval.apply(rule.form(list.toArray())).toInt();
	}

	@Override
	public final Object invoke(String name, Object...args) {
		return eval.apply(new ElvaName(name).form(args)).value();
	}
}
