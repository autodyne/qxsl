/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Reader;
import java.io.StringReader;
import javax.script.Bindings;
import javax.script.ScriptException;
import javax.xml.namespace.QName;

import elva.bind.Local;
import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.lang.ElvaRuntime;

import qxsl.model.Item;
import qxsl.model.Tuple;

/**
 * {@link Contest}をLISPベースのドメイン特化言語で表現する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 *
 * @see ElvaRuntime 内部で使用されるLISP処理系
 */
public final class RuleKit {
	private final ElvaRuntime elva;

	/**
	 * LISP処理系を構築します。
	 */
	public RuleKit() {
		this(RuleKit.class.getClassLoader());
	}

	/**
	 * LISP処理系を構築します。
	 *
	 * @param loader パッケージを検索するクラスローダ
	 */
	public RuleKit(ClassLoader loader) {
		this.elva = new ElvaRuntime(loader);
	}

	/**
	 * LISP処理系が内蔵する関数や値を参照する環境を返します。
	 *
	 * @return 事前に定義された環境
	 */
	private final Bindings createBindings() {
		final Local env = new Local(null);
		env.put(new Contest.$Contest());
		env.put(new Section.$Section());
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
			final Item item = eval.apply(args.get(0)).ofClass(Item.class);
			final int score = eval.apply(args.get(1)).ival();
			final var mults = args.drop(2).map(eval).stream();
			return new Success(item, score, mults.toArray());
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
			final Item item = eval.apply(args.get(0)).ofClass(Item.class);
			final String ms = eval.apply(args.get(1)).text();
			return new Failure(item, ms);
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
			return eval.apply(args.head()).ofClass(Item.class).getRcvd();
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
			return eval.apply(args.head()).ofClass(Item.class).getSent();
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
			return tuple.ofClass(Tuple.class).value(qname);
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
			return tuple.ofClass(Tuple.class).set(qname, value);
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
	 * @throws ClassCastException 返り値が不正な型の場合
	 * @throws ScriptException 式の評価時に発生する例外
	 */
	public Contest contest(Reader reader) throws ScriptException {
		return (Contest) elva.eval(reader, createBindings());
	}

	/**
	 * 指定された文字列から式を読み取って評価します。
	 * 返り値はコンテストの定義である必要があります。
	 *
	 *
	 * @param string 式
	 * @return コンテストの定義
	 *
	 * @throws ClassCastException 返り値が不正な型の場合
	 * @throws ScriptException 式の評価時に発生する例外
	 */
	public Contest contest(String string) throws ScriptException {
		return contest(new StringReader(string));
	}
}
