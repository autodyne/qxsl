/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import elva.*;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.StringJoiner;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptException;
import javax.script.SimpleBindings;
import javax.script.SimpleScriptContext;

import qxsl.extra.field.qxsl.*;
import qxsl.model.Exch;
import qxsl.model.Item;

import static elva.ElvaScriptEngine.Lisp;
import static elva.ElvaScriptEngine.Seq;
import static javax.script.ScriptContext.ENGINE_SCOPE;

/**
 * {@link Contest}をLISPベースのドメイン特化言語で表現する仕組みです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/27
 *
 * @see ElvaScriptEngine 内部で使用されるLISP処理系
 */
public final class RuleKit {
	private final ElvaScriptEngine elva;
	private final ScriptContext context;

	/**
	 * LISP処理系を構築します。
	 */
	public RuleKit() {
		this.elva = new ElvaScriptEngine();
		context = new SimpleScriptContext();
		context.setBindings(createBindings(), ENGINE_SCOPE);
	}

	/**
	 * 指定された入力から文字列を読み取り評価します。
	 * 返り値はコンテストの定義である必要があります。
	 * 
	 * 
	 * @param reader 式を読み取るリーダ
	 * @return コンテストの定義
	 *
	 * @throws ClassCastException 返り値が不正な型の場合
	 * @throws ScriptException 式の評価時に発生する例外
	 */
	public Contest eval(Reader reader) throws ScriptException {
		return (Contest) this.elva.eval(reader, this.context);
	}

	/**
	 * 指定された入力から文字列を読み取り評価します。
	 * 返り値はコンテストの定義である必要があります。
	 * 
	 * 
	 * @param string 式を読み取る文字列
	 * @return コンテストの定義
	 *
	 * @throws ClassCastException 返り値が不正な型の場合
	 * @throws ScriptException 式の評価時に発生する例外
	 */
	public Contest eval(String string) throws ScriptException {
		return (Contest) this.elva.eval(string, this.context);
	}

	/**
	 * LISP処理系内部における{@link Contest}の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/20
	 */
	private static final class ContestImpl extends Contest {
		private final String name;
		private final List<Section> sections;
		/**
		 * 指定された識別名と部門集合で部門を構築します。
		 *
		 * @param ident このコンテストの識別名
		 * @param sects このコンテストの部門
		 */
		public ContestImpl(String ident, List<Section> sects) {
			this.name = ident;
			this.sections = Collections.unmodifiableList(sects);
		}
		@Override
		public String getName() {
			return name;
		}
		@Override
		public java.util.Iterator<Section> iterator() {
			return sections.iterator();
		}
	}

	/**
	 * LISP処理系内部における{@link Section}の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/20
	 */
	private static final class SectionImpl extends Section {
		private final String name;
		private final Lisp eval;
		private final Function rule;
		/**
		 * 指定された名前と規約で部門を構築します。
		 *
		 * @param name 部門名
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public SectionImpl(String name, Function rule, Lisp eval) {
			this.name = name;
			this.rule = rule;
			this.eval = eval;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public Message validate(Item item) throws ScriptException {
			Object sexp = eval.eval(new Seq(rule, item));
			if(sexp instanceof Success) return (Success) sexp;
			if(sexp instanceof Failure) return (Failure) sexp;
			String temp = "%s must return  a success or failure";
			throw new ScriptException(String.format(temp, rule));
		}
	}

	/**
	 * LISP処理系で事前に定義されるcontest関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/15
	 */
	@Arguments(min = 1, max = -1)
	private static final class $Contest implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final String contest = eval.text(args.car());
			ArrayList<Section> sects = new ArrayList<>();
			ArrayList<String> errors = new ArrayList<>();
			for(Object arg: args.cdr()) {
				final Object o = eval.eval(arg);
				if(o instanceof Section) sects.add((Section) o);
				else errors.add(String.format("%s must be a section", o));
			}
			if(errors.isEmpty()) return new ContestImpl(contest, sects);
			final String temp = "contest construction failed by:{";
			final StringJoiner sj = new StringJoiner("\n", temp, "\n}");
			for(String msg: errors) sj.add(msg);
			throw new ScriptException(sj.toString());
		}
	}

	/**
	 * LISP処理系で事前に定義されるsection関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/15
	 */
	@Arguments(min = 2, max = 2)
	private static final class $Section implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final String section = eval.text(args.car());
			final Function body = eval.func(args.get(1));
			return new SectionImpl(section, body, eval);
		}
	}

	/**
	 * LISP処理系で事前に定義されるsuccess関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 3, max = -1)
	private static final class $Success implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object item = eval.eval(args.get(0));
			final int score = eval.integer(args.get(1));
			ArrayList<Object> klist = new ArrayList<>();
			for(Object k: args.cdr().cdr()) klist.add(eval.eval(k));
			final Object[] ks = klist.toArray();
			if(item instanceof Item) return new Success(score, (Item) item, ks);
			throw new ScriptException("make sure (success ITEM score keys...)");
		}
	}

	/**
	 * LISP処理系で事前に定義されるfailure関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 2, max = 2)
	private static final class $Failure implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object item = eval.eval(args.get(0));
			final String text = eval.text(args.get(1));
			if(item instanceof Item) return new Failure(text, (Item) item);
			throw new ScriptException("make sure (failure ITEM message)");
		}
	}

	/**
	 * LISP処理系で事前に定義されるrcvd関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Rcvd implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object item = eval.eval(args.car());
			if(item instanceof Item) return ((Item) item).getRcvd();
			throw new ScriptException(String.format("%s is not an Item", item));
		}
	}

	/**
	 * LISP処理系で事前に定義されるsent関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Sent implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object item = eval.eval(args.car());
			if(item instanceof Item) return ((Item) item).getSent();
			throw new ScriptException(String.format("%s is not an Item", item));
		}
	}

	/**
	 * LISP処理系で事前に定義されるhour関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Hour implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object item = eval.eval(args.car());
			if(item instanceof Item) return ((Item) item).get(Time.class).hour();
			throw new ScriptException(String.format("%s is not an Item", item));
		}
	}

	/**
	 * LISP処理系で事前に定義されるcall関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Call implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object item = eval.eval(args.car());
			if(item instanceof Item) return ((Item) item).value(Call.class);
			throw new ScriptException(String.format("%s is not an Item", item));
		}
	}

	/**
	 * LISP処理系で事前に定義されるband関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Band implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object item = eval.eval(args.car());
			if(item instanceof Item) return ((Item) item).get(Band.class).toInt();
			throw new ScriptException(String.format("%s is not an Item", item));
		}
	}

	/**
	 * LISP処理系で事前に定義されるmode関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Mode implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object item = eval.eval(args.car());
			if(item instanceof Item) return ((Item) item).value(Mode.class);
			throw new ScriptException(String.format("%s is not an Item", item));
		}
	}

	/**
	 * LISP処理系で事前に定義されるrstq関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $RSTQ implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object exch = eval.eval(args.car());
			if(exch instanceof Exch) return ((Exch) exch).value(RSTQ.class);
			throw new ScriptException(String.format("%s is not an Exch", exch));
		}
	}

	/**
	 * LISP処理系で事前に定義されるcode関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Code implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object exch = eval.eval(args.car());
			if(exch instanceof Exch) return ((Exch) exch).value(Code.class);
			throw new ScriptException(String.format("%s is not an Exch", exch));
		}
	}

	/**
	 * LISP処理系で事前に定義されるcity関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $City implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final String base = eval.text(args.car());
			final String code = eval.text(args.get(1));
			final int level = eval.integer(args.get(2));
			return new City(base, code).getName(level);
		}
	}

	/**
	 * LISP処理系で事前に定義された関数や値を保持する環境を作成します。
	 *
	 * @return 事前に定義された環境
	 */
	public Bindings createBindings() {
		final Bindings lude = new SimpleBindings();
		/*
		 * preinstalled functions for contest & section definition
		 * 
		 * (contest symbol-expression sections...)
		 * (special symbol-expression lambda)
		 */
		lude.put("contest", new $Contest());
		lude.put("section", new $Section());

		/*
		 * preinstalled functions for success & failure construction
		 * 
		 * (success ITEM score keys...)
		 * (failure ITEM message)
		 */
		lude.put("success", new $Success());
		lude.put("failure", new $Failure());

		/*
		 * preinstalled functions for rcvd & sent access
		 *
		 * (rcvd item-expression)
		 * (sent item-expression)
		 */
		lude.put("rcvd", new $Rcvd());
		lude.put("sent", new $Sent());

		/*
		 * preinstalled functions for field access
		 *
		 * (hour item-expression)
		 * (call item-expression)
		 * (band item-expression)
		 * (mode item-expression)
		 */
		lude.put("hour", new $Hour());
		lude.put("call", new $Call());
		lude.put("band", new $Band());
		lude.put("mode", new $Mode());

		/*
		 * preinstalled functions for rcvd / sent field access
		 *
		 * (rstq exch-expression)
		 * (code exch-expression)
		 *
		 * @since 2016/11/25
		 */
		lude.put("rstq", new $RSTQ());
		lude.put("code", new $Code());

		/*
		 * preinstalled functions for city access
		 *
		 * (city database-name code region-level)
		 */
		lude.put("city", new $City());
		return lude;
	}
}
