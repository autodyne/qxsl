/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.ruler;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import javax.xml.namespace.QName;

import elva.bind.Local;
import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.lang.ElvaRuntime;

import qxsl.model.Item;
import qxsl.model.Tuple;
import qxsl.ruler.Contest;
import qxsl.ruler.Failure;
import qxsl.ruler.Success;

/**
 * ドメイン特化のLISPでコンテストの規約を表現する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 *
 * @see ElvaRuntime 内部で使用されるLISP処理系
 */
public final class ElvaRuleKit extends qxsl.ruler.RuleKit {
	private final ScriptEngine engine;

	/**
	 * LISP処理系を構築します。
	 */
	public ElvaRuleKit() {
		super("elva");
		this.engine = new ElvaRuntime();
	}

	/**
	 * LISP処理系が内蔵する関数や値を参照する環境を返します。
	 *
	 * @return 事前に定義された環境
	 */
	private final Local createBindings() {
		final Local env = new Local(null);
		env.put(new $Contest());
		env.put(new $Section());
		env.put(new $Success());
		env.put(new $Failure());
		env.put(new $Item());
		env.put(new $Rcvd());
		env.put(new $Sent());
		env.put(new $GetField());
		env.put(new $SetField());
		return env;
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
		public Object apply(ElvaList args, ElvaEval eval) {
			return new ElvaContest(args, eval);
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
	@ElvaForm.Parameters(min = 3, max = 3)
	static final class $Section extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return new ElvaSection(args, eval);
		}
	}

	/**
	 * この関数は交信記録の検査結果に合格の注釈を付けます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@ElvaForm.Native("success")
	@ElvaForm.Parameters(min = 3, max = -1)
	private static final class $Success extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var valid = eval.apply(args.get(0));
			final int score = eval.apply(args.get(1)).toInt();
			final var mults = args.drop(2).map(eval).stream();
			final var tuple = valid.ofType(Item.class);
			return new Success(tuple, score, mults.toArray());
		}
	}

	/**
	 * この関数は交信記録の検査結果に不可の注釈を付けます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@ElvaForm.Native("failure")
	@ElvaForm.Parameters(min = 2, max = 2)
	private static final class $Failure extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var bad = eval.apply(args.get(0));
			final var msg = eval.apply(args.get(1)).value();
			return new Failure(bad.ofType(Item.class), msg);
		}
	}

	/**
	 * この関数は交信記録の項目を生成します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@ElvaForm.Native("item")
	@ElvaForm.Parameters(min = 0, max = 0)
	private static final class $Item extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return new Item();
		}
	}

	/**
	 * この関数は交信相手局が送信した情報を取り出します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@ElvaForm.Native("rcvd")
	@ElvaForm.Parameters(min = 1, max = 1)
	private static final class $Rcvd extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head()).ofType(Item.class).getRcvd();
		}
	}

	/**
	 * この関数は交信相手局に送信した情報を取り出します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@ElvaForm.Native("sent")
	@ElvaForm.Parameters(min = 1, max = 1)
	private static final class $Sent extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head()).ofType(Item.class).getSent();
		}
	}

	/**
	 * この関数は交信記録の属性の設定値を取り出します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/06/29
	 */
	@ElvaForm.Native("get-field")
	@ElvaForm.Parameters(min = 3, max = 3)
	private static final class $GetField extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var tuple = eval.apply(args.head());
			final var space = eval.apply(args.get(1)).text();
			final var local = eval.apply(args.get(2)).text();
			final var qname = new QName(space, local);
			return tuple.ofType(Tuple.class).value(qname);
		}
	}

	/**
	 * この関数は交信記録の属性の設定値を上書きします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/06/29
	 */
	@ElvaForm.Native("set-field")
	@ElvaForm.Parameters(min = 4, max = 4)
	private static final class $SetField extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var tuple = eval.apply(args.head());
			final var space = eval.apply(args.get(1)).text();
			final var local = eval.apply(args.get(2)).text();
			final var value = eval.apply(args.get(3)).text();
			final var qname = new QName(space, local);
			return tuple.ofType(Tuple.class).set(qname, value);
		}
	}

	/**
	 * 指定されたリーダから式を読み取って評価します。
	 * 返り値はコンテストの定義である必要があります。
	 *
	 *
	 * @param reader 式を読み取るリーダ
	 * @return コンテストの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	@Override
	public final Contest contest(final Reader reader) {
		try {
			return (Contest) engine.eval(reader, createBindings());
		} catch (ScriptException ex) {
			throw new UncheckedIOException(new IOException(ex));
		}
	}

	/**
	 * 指定された文字列から式を読み取って評価します。
	 * 返り値はコンテストの定義である必要があります。
	 *
	 *
	 * @param string 式
	 * @return コンテストの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	@Override
	public final Contest contest(final String string) {
		return contest(new StringReader(string));
	}
}
