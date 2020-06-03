/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.function.Function;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.core.ElvaName;

import qxsl.model.Item;

/**
 * コンテストの部門の実装は{@link Section}クラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Section implements Function<Item, Message> {
	/**
	 * 部門を構築します。
	 */
	public Section() {}

	/**
	 * 部門の名前を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * 部門の名前を返します。
	 *
	 * @return 部門の名前
	 */
	public abstract String getName();

	/**
	 * 部門の符牒を返します。
	 *
	 * @return 部門の符牒
	 */
	public abstract String getCode();

	/**
	 * この部門を有するコンテストを返します。
	 *
	 * @return 部門を内包するコンテスト
	 */
	public abstract Contest getContest();

	/**
	 * 指定された{@link Item}の可否を検査します。
	 *
	 * @param item 検査対象の交信記録
	 * @return 承認された場合はtrue
	 */
	public abstract Message apply(Item item);

	/**
	 * この部門に紐づけられた関数を実行します。
	 *
	 * @param name 関数の名前
	 * @param args 関数の引数
	 * @return 関数の値
	 *
	 * @since 2020/03/09
	 */
	public abstract Object invoke(String name, Object...args);

	/**
	 * LISP処理系内部における{@link Section}の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/20
	 */
	private static final class SectionKit extends Section {
		private final Contest test;
		private final String name;
		private final String code;
		private final ElvaForm rule;
		private final ElvaEval eval;

		/**
		 * 指定された部門定義と評価器で部門を構築します。
		 *
		 * @param rule 部門
		 * @param eval 評価器
		 */
		public SectionKit(ElvaList rule, ElvaEval eval) {
			this.test = eval.apply(rule.get(0)).ofClass(Contest.class);
			this.name = eval.apply(rule.get(1)).text();
			this.code = eval.apply(rule.get(2)).text();
			this.rule = eval.apply(rule.get(3)).form();
			this.eval = eval;
			this.test.add(this);
		}

		@Override
		public String getName() {
			return this.name;
		}

		@Override
		public String getCode() {
			return this.code;
		}

		@Override
		public Contest getContest() {
			return this.test;
		}

		@Override
		public Message apply(Item item) {
			ElvaList list = ElvaList.array(Arrays.asList(rule, item));
			return this.eval.apply(list).ofClass(Message.class);
		}

		@Override
		public Object invoke(String name, Object...args) {
			return eval.apply(new ElvaName(name).chain(args)).value();
		}
	}

	/**
	 * この関数はコンテストの部門の実体を生成します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/15
	 */
	@ElvaForm.Native("section")
	@ElvaForm.Parameters(min = 4, max = 4)
	static final class $Section extends ElvaForm {
		public Contest apply(ElvaList args, ElvaEval eval) {
			return new SectionKit(args, eval).getContest();
		}
	}

	/**
	 * 指定された交信記録から有効な交信を抽出します。
	 *
	 * @param items 交信記録
	 * @return 得点計算の結果
	 *
	 * @since 2019/05/16
	 */
	public final Summary summarize(Collection<Item> items) {
		final var acc = new ArrayList<Success>();
		final var rej = new ArrayList<Failure>();
		for(Item item: items) {
			final Message msg = this.apply(item);
			if(msg instanceof Success) acc.add((Success) msg);
			if(msg instanceof Failure) rej.add((Failure) msg);
		}
		return new Summary(acc, rej).confirm(this);
	}
}
