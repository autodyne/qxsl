/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.UnaryOperator;

import elva.Cons;
import elva.Eval;
import elva.Form;
import elva.Name;
import elva.Sexp;

import qxsl.model.Item;

/**
 * 交信記録への処理の実装は{@link Handler}クラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/26
 */
public abstract class Handler implements UnaryOperator<Item> {
	/**
	 * 処理を構築します。
	 */
	public Handler() {}

	/**
	 * 処理の名前を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * 処理の名前を返します。
	 *
	 * @return 処理の名前
	 */
	public abstract String getName();

	/**
	 * 指定された交信記録を処理します。
	 *
	 * @param item 処理対象の交信記録
	 * @return 処理の返り値
	 */
	public abstract Item apply(Item item);

	/**
	 * この処理に紐づけられた関数を実行します。
	 *
	 * @param name 関数の名前
	 * @param args 関数の引数
	 * @return 関数の値
	 *
	 * @since 2020/03/09
	 */
	public abstract Object invoke(String name, Object...args);

	/**
	 * LISP処理系内部における{@link Handler}の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	private static final class HandlerKit extends Handler {
		private final String name;
		private final Form rule;
		private final Eval eval;

		/**
		 * 指定された処理定義と評価器で処理を構築します。
		 *
		 * @param rule 処理
		 * @param eval 評価器
		 */
		public HandlerKit(Cons rule, Eval eval) {
			this.name = eval.apply(rule.get(0)).text();
			this.rule = eval.apply(rule.get(1)).form();
			this.eval = eval;
		}

		@Override
		public String getName() {
			return this.name;
		}

		@Override
		public Item apply(Item item) {
			return eval.apply(Cons.wrap(rule, item)).value(Item.class);
		}

		@Override
		public Object invoke(String name, Object...args) {
			return eval.apply(Name.list(name, args)).value();
		}
	}

	/**
	 * この関数は交信記録への処理の実体を生成します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("handler")
	@Form.Parameters(min = 2, max = 2)
	static final class $Handler extends Form {
		public Handler apply(Cons args, Eval eval) {
			return new HandlerKit(args, eval);
		}
	}

	/**
	 * 指定された交信記録に対して処理を実行します。
	 *
	 * @param items 交信記録
	 * @return 処理の結果
	 *
	 * @since 2020/03/09
	 */
	public final List<Item> handle(Collection<Item> items) {
		final var target = new ArrayList<Item>();
		for(var it: items) target.add(apply(it));
		return Collections.unmodifiableList(target);
	}

	/**
	 * このライブラリに同梱されたフォーマット処理の定義のパスです。
	 *
	 * @since 2020/03/09
	 */
	public static final String FORMAT = "qxsl/ruler/format.lisp";
}
