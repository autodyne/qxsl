/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.ruler;

import elva.lang.ElvaEval;
import elva.lang.ElvaLisp;
import elva.lang.ListBase;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;
import elva.lang.NativeOp;
import elva.lang.ScopeMap;
import elva.warn.ElvaLexicalException;
import elva.warn.ElvaRuntimeException;
import qxsl.model.Item;
import qxsl.model.Tuple;
import qxsl.ruler.Contest;
import qxsl.ruler.Failure;
import qxsl.ruler.Success;

import javax.script.ScriptException;
import javax.xml.namespace.QName;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;

/**
 * ドメイン特化のLISPでコンテストの規約を表現する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 *
 * @see ElvaLisp 内部で使用されるLISP処理系
 */
public final class ElvaRuleKit extends qxsl.ruler.RuleKit {
	private final ElvaLisp engine;

	/**
	 * LISP処理系を構築します。
	 */
	public ElvaRuleKit() {
		super("elva");
		this.engine = new ElvaLisp();
	}

	/**
	 * LISP処理系が内蔵する関数や値を参照する環境を返します。
	 *
	 * @return 事前に定義された環境
	 */
	private final ScopeMap createBindings() {
		final var env = (ScopeMap) engine.createBindings();
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
	@Name("contest")
	@Args(min = 2, max = 2)
	private static final class $Contest extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
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
	@Name("section")
	@Args(min = 3, max = 3)
	private static final class $Section extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
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
	@Name("success")
	@Args(min = 3, max = -1)
	private static final class $Success extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
			final var valid = eval.apply(args.get(0));
			final int score = eval.apply(args.get(1)).real().toInt();
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
	@Name("failure")
	@Args(min = 2, max = 2)
	private static final class $Failure extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
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
	@Name("item")
	@Args(min = 0, max = 0)
	private static final class $Item extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
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
	@Name("rcvd")
	@Args(min = 1, max = 1)
	private static final class $Rcvd extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
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
	@Name("sent")
	@Args(min = 1, max = 1)
	private static final class $Sent extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
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
	@Name("get-field")
	@Args(min = 3, max = 3)
	private static final class $GetField extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
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
	@Name("set-field")
	@Args(min = 4, max = 4)
	private static final class $SetField extends NativeOp {
		public Object apply(ListBase args, ElvaEval eval) {
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
	 * @throws UncheckedIOException 式の読み取りの例外
	 * @throws ElvaLexicalException 式の構文面での例外
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	@Override
	public final Contest contest(final Reader reader) {
		try {
			final var binds = createBindings();
			return (Contest) engine.eval(reader, binds);
		} catch (ScriptException ex) {
			throw new ElvaRuntimeException(ex);
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
	 * @throws UncheckedIOException 式の読み取りの例外
	 * @throws ElvaLexicalException 式の構文面での例外
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	@Override
	public final Contest contest(final String string) {
		return contest(new StringReader(string));
	}
}
