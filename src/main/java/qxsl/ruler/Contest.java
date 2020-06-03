/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.core.ElvaName;
import elva.core.ElvaNode;

/**
 * コンテストの規約の実装は{@link Contest}クラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Contest implements Iterable<Section> {
	/**
	 * 規約を構築します。
	 */
	public Contest() {}

	/**
	 * コンテストの名前を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * コンテストの名前を返します。
	 *
	 * @return コンテストの名前
	 */
	public abstract String getName();

	/**
	 * このコンテストの部門をイテレータで返します。
	 *
	 * @return 全ての部門を含むイテレータ
	 */
	public abstract Iterator<Section> iterator();

	/**
	 * 指定された交信記録の総得点を計算します。
	 *
	 * @param sum 交信記録
	 * @return 総得点
	 *
	 * @since 2020/02/26
	 */
	public abstract int score(Summary sum);

	/**
	 * このコンテストに紐づけられた関数を実行します。
	 *
	 * @param name 関数の名前
	 * @param args 関数の引数
	 * @return 関数の値
	 *
	 * @since 2020/03/09
	 */
	public abstract Object invoke(String name, Object...args);

	/**
	 * 指定された部門をこのコンテストに追加します。
	 *
	 * @param section 追加する部門
	 * @return この部門
	 */
	protected final Contest add(Section section) {
		((ContestKit) this).list.add(section);
		return this;
	}

	/**
	 * LISP処理系内部における{@link Contest}の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/20
	 */
	private static final class ContestKit extends Contest {
		private final List<Section> list;
		private final String name;
		private final ElvaForm rule;
		private final ElvaEval eval;

		/**
		 * 指定された規約定義と評価器で規約を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public ContestKit(ElvaList rule, ElvaEval eval) {
			this.name = eval.apply(rule.get(0)).text();
			this.rule = eval.apply(rule.get(1)).form();
			this.list = new ArrayList<>();
			this.eval = eval;
		}

		@Override
		public String getName() {
			return this.name;
		}

		@Override
		public Iterator<Section> iterator() {
			return this.list.iterator();
		}

		@Override
		public final int score(final Summary sum) {
			if(sum.accepted().isEmpty()) return 0;
			final var seq = new ArrayList<ElvaNode>();
			seq.add(ElvaNode.wrap(rule));
			seq.add(ElvaNode.wrap(sum.score()));
			for(var m: sum.mults()) seq.add(ElvaList.chain(m));
			return this.eval.apply(ElvaList.chain(seq)).ival();
		}

		@Override
		public Object invoke(String name, Object...args) {
			return eval.apply(new ElvaName(name).chain(args)).value();
		}
	}

	/**
	 * この関数はコンテストの規約の実体を生成します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/15
	 */
	@ElvaForm.Native("contest")
	@ElvaForm.Parameters(min = 2, max = 2)
	static final class $Contest extends ElvaForm {
		public Contest apply(ElvaList args, ElvaEval eval) {
			return new ContestKit(args, eval);
		}
	}

	/**
	 * 指定された名前の部門を返します。未知の場合は例外を投げます。
	 *
	 * @param name 部門の名前
	 * @return 該当する部門
	 *
	 * @throws NoSuchElementException 未知の部門の場合
	 */
	public final Section getSection(String name) {
		for(Section s: this) if(s.toString().equals(name)) return s;
		throw new NoSuchElementException(name.concat(" not found"));
	}

	/**
	 * このライブラリに同梱されたALLJA1コンテストの規約のパスです。
	 *
	 * @since 2020/03/09
	 */
	public static final URL ALLJA1 = Contest.class.getResource("allja1.lisp");
}
