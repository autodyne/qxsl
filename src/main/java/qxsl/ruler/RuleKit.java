/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import javax.script.*;
import javax.xml.namespace.QName;

import elva.*;
import elva.Form.Lambda;
import elva.Form.Syntax;

import qxsl.extra.field.City;
import qxsl.extra.field.Time;
import qxsl.model.Exch;
import qxsl.model.Item;
import qxsl.model.Tuple;

import static elva.Elva.ElvaRuntimeException;
import static java.nio.charset.StandardCharsets.UTF_8;
import static javax.script.ScriptContext.ENGINE_SCOPE;

/**
 * {@link Contest}をLISPベースのドメイン特化言語で表現する仕組みです。
 * 
 * 
 * @author 無線部開発班
 *
 * @since 2017/02/27
 *
 * @see Elva 内部で使用されるLISP処理系
 */
public final class RuleKit {
	private final Elva elva;
	private final ScriptContext context;

	/**
	 * LISP処理系を構築します。
	 */
	public RuleKit() {
		this.elva = new Elva();
		context = new SimpleScriptContext();
		context.setBindings(createBindings(), ENGINE_SCOPE);
	}

	/**
	 * 指定された入力から文字列を読み取り評価します。
	 * 返り値は交信記録の手続きである必要があります。
	 * 
	 * 
	 * @param reader 式を読み取るリーダ
	 * @return 手続きの定義
	 *
	 * @throws ClassCastException 返り値が不正な型の場合
	 * @throws ScriptException 式の評価時に発生する例外
	 *
	 * @since 2020/02/26
	 */
	public Handler handler(Reader reader) throws ScriptException {
		return (Handler) this.elva.eval(reader, this.context);
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
	public Contest contest(Reader reader) throws ScriptException {
		return (Contest) this.elva.eval(reader, this.context);
	}

	/**
	 * 指定された{@link Handler}をライブラリから読み出します。
	 *
	 * @param name ハンドラを定義したファイルの名前
	 * @return ライブラリに内蔵されたハンドラの定義
	 * 
	 * @throws ScriptException ハンドラ定義読み取り時の例外
	 */
	public Handler handler(String name) throws ScriptException {
		try(var is = getClass().getResourceAsStream(name)) {
			return handler(new InputStreamReader(is, UTF_8));
		} catch(IOException ex) {
			throw new ScriptException(ex);
		}
	}

	/**
	 * 指定された{@link Contest}をライブラリから読み出します。
	 *
	 * @param name コンテストを定義したファイルの名前
	 * @return ライブラリに内蔵されたコンテストの定義
	 * 
	 * @throws ScriptException コンテスト定義読み取り時の例外
	 */
	public Contest contest(String name) throws ScriptException {
		try(var is = getClass().getResourceAsStream(name)) {
			return contest(new InputStreamReader(is, UTF_8));
		} catch(IOException ex) {
			throw new ScriptException(ex);
		}
	}

	/**
	 * LISP処理系内部における{@link Handler}の実装です。
	 * 
	 * 
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	private static final class HandlerImpl extends Handler {
		private final String name;
		private final Lambda rule;
		private final Eval eval;

		/**
		 * 指定された規約定義と評価器で部門を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public HandlerImpl(Cons rule, Eval eval) {
			this.name = eval.text(rule.get(0));
			this.rule = eval.eval(rule.get(1)).as(Lambda.class);
			this.eval = eval;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public Item apply(Item item) throws RuntimeException {
			return eval.eval(Cons.wrap(rule, item)).as(Item.class);
		}
	}

	/**
	 * LISP処理系内部における{@link Contest}の実装です。
	 * 
	 * 
	 * @author 無線部開発班
	 *
	 * @since 2017/02/20
	 */
	private static final class ContestImpl extends Contest {
		private final String name;
		private final Syntax rule;
		private final Eval eval;
		private final List<Section> list;

		/**
		 * 指定された規約定義と評価器で規約を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public ContestImpl(Cons rule, Eval eval) {
			this.name = eval.text(rule.get(0));
			this.rule = eval.eval(rule.get(1)).as(Syntax.class);
			this.list = SectionImpl.sects(rule.cdr(2), eval);
			this.eval = eval;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public java.util.Iterator<Section> iterator() {
			return list.iterator();
		}

		@Override
		public int score(Summary sum) throws RuntimeException {
			if (sum.score() > 0) {
				final var args = new ArrayList<Sexp>();
				args.add(Sexp.wrap(rule));
				args.add(Sexp.wrap(sum.score()));
				for (var ms: sum.mults()) args.add(Cons.wrap(ms));
				return eval.real(Cons.cons(args)).intValueExact();
			} else return 0;
		}
	}

	/**
	 * LISP処理系内部における{@link Section}の実装です。
	 * 
	 * 
	 * @author 無線部開発班
	 *
	 * @since 2017/02/20
	 */
	private static final class SectionImpl extends Section {
		private final String name;
		private final String code;
		private final Eval eval;
		private final Lambda rule;

		/**
		 * 指定された規約定義と評価器で部門を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public SectionImpl(Cons rule, Eval eval) {
			this.name = eval.text(rule.get(0));
			this.code = eval.text(rule.get(1));
			this.rule = eval.eval(rule.get(2)).as(Lambda.class);
			this.eval = eval;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public String getCode() {
			return code;
		}

		@Override
		public Message apply(Item item) throws RuntimeException {
			return eval.eval(Cons.wrap(rule, item)).as(Message.class);
		}

		/**
		 * 指定されたリストを部門の列挙として評価します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 * @return 部門のリスト
		 */
		private static List<Section> sects(Cons rule, Eval eval) {
			final var list = new ArrayList<Section>();
			for(var sc: rule) list.add(eval.eval(sc).as(Section.class));
			return Collections.unmodifiableList(list);
		}
	}

	/**
	 * LISP処理系で事前に定義された関数や値を保持する環境を作成します。
	 *
	 * @return 事前に定義された環境
	 */
	public final Bindings createBindings() {
		final Nest env = new Nest(null, null);
		/*
		 * preinstalled functions to load script
		 * 
		 * (load file)
		 */
		env.put(new $Load());

		/*
		 * preinstalled functions for contest & handler definition
		 * 
		 * (handler handler-name lambda)
		 * (contest contest-name syntax sections ...)
		 * (section section-name section-code lambda)
		 */
		env.put(new $Handler());
		env.put(new $Contest());
		env.put(new $Section());

		/*
		 * preinstalled functions for success & failure construction
		 * 
		 * (success ITEM score keys...)
		 * (failure ITEM message)
		 */
		env.put(new $Success());
		env.put(new $Failure());

		/*
		 * preinstalled functions for item creation
		 *
		 * (item)
		 */
		env.put(new $Item());

		/*
		 * preinstalled functions for rcvd & sent access
		 *
		 * (rcvd item)
		 * (sent item)
		 */
		env.put(new $Rcvd());
		env.put(new $Sent());

		/*
		 * preinstalled functions for field access
		 *
		 * (get-field item namespace name)
		 * (set-field item namespace name value-string)
		 */
		env.put(new $GetField());
		env.put(new $SetField());

		/*
		 * preinstalled functions for time access
		 *
		 * (hour item)
		 */
		env.put(new $Hour());

		/*
		 * preinstalled functions for city access
		 *
		 * (city database-name code region-level)
		 */
		env.put(new $City());
		return env;
	}

	/**
	 * LISP処理系で事前に定義されるload関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("load")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Load extends Form {
		public Object apply(Cons args, Eval eval) {
			Sexp value = null;
			final var elva = new Elva();
			final var name = eval.text(args.car());
			try (var is = getClass().getResourceAsStream(name)) {
				final var isr = new InputStreamReader(is, UTF_8);
				for(Sexp s: elva.scan(isr)) value = eval.eval(s);
				return value;
			} catch (IOException | ScriptException ex) {
				final String msg = "failed in loading %s: %s";
				throw new ElvaRuntimeException(msg, name, ex);
			}
		}
	}

	/**
	 * LISP処理系で事前に定義されるhandler関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("handler")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Handler extends Form {
		public Object apply(Cons args, Eval eval) {
			return new HandlerImpl(args, eval);
		}
	}

	/**
	 * LISP処理系で事前に定義されるcontest関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/15
	 */
	@Form.Native("contest")
	@Form.Parameters(min = 3, max = -1)
	private static final class $Contest extends Form {
		public Object apply(Cons args, Eval eval) {
			return new ContestImpl(args, eval);
		}
	}

	/**
	 * LISP処理系で事前に定義されるsection関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/15
	 */
	@Form.Native("section")
	@Form.Parameters(min = 3, max = 3)
	private static final class $Section extends Form {
		public Object apply(Cons args, Eval eval) {
			return new SectionImpl(args, eval);
		}
	}

	/**
	 * LISP処理系で事前に定義されるsuccess関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@Form.Native("success")
	@Form.Parameters(min = 3, max = -1)
	private static final class $Success extends Form {
		public Object apply(Cons args, Eval eval) {
			final var it = eval.eval(args.get(0)).as(Item.class);
			final var sc = eval.real(args.get(1));
			final var ks = args.cdr().cdr().stream().map(eval::eval);
			return new Success(it, sc.intValueExact(), ks.toArray());
		}
	}

	/**
	 * LISP処理系で事前に定義されるfailure関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@Form.Native("failure")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Failure extends Form {
		public Object apply(Cons args, Eval eval) {
			final Item item = eval.eval(args.car()).as(Item.class);
			return new Failure(item, eval.text(args.get(1)));
		}
	}

	/**
	 * LISP処理系で事前に定義されるitem関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("item")
	@Form.Parameters(min = 0, max = 0)
	private static final class $Item extends Form {
		public Object apply(Cons args, Eval eval) {
			return new Item();
		}
	}

	/**
	 * LISP処理系で事前に定義されるrcvd関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@Form.Native("rcvd")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Rcvd extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car()).as(Item.class).getRcvd();
		}
	}

	/**
	 * LISP処理系で事前に定義されるsent関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@Form.Native("sent")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Sent extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car()).as(Item.class).getSent();
		}
	}

	/**
	 * LISP処理系で事前に定義されるget-field関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/06/29
	 */
	@Form.Native("get-field")
	@Form.Parameters(min = 3, max = 3)
	private static final class $GetField extends Form {
		public Object apply(Cons args, Eval eval) {
			final var tuple = eval.eval(args.get(0));
			final var space = eval.text(args.get(1));
			final var local = eval.text(args.get(2));
			final var qname = new QName(space, local);
			return tuple.as(Tuple.class).value(qname);
		}
	}

	/**
	 * LISP処理系で事前に定義されるset-field関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/06/29
	 */
	@Form.Native("set-field")
	@Form.Parameters(min = 4, max = 4)
	private static final class $SetField extends Form {
		public Object apply(Cons args, Eval eval) {
			final var tuple = eval.eval(args.get(0));
			final var space = eval.text(args.get(1));
			final var local = eval.text(args.get(2));
			final var value = eval.text(args.get(3));
			final var qname = new QName(space, local);
			return tuple.as(Tuple.class).set(qname, value);
		}
	}

	/**
	 * LISP処理系で事前に定義されるhour関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@Form.Native("hour")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Hour extends Form {
		public Object apply(Cons args, Eval eval) {
			final var time = eval.eval(args.get(0));
			final var text = eval.text(args.get(1));
			final var date = time.as(ZonedDateTime.class);
			ZoneId zone = ZoneId.of(text, ZoneId.SHORT_IDS);
			return date.withZoneSameInstant(zone).getHour();
		}
	}

	/**
	 * LISP処理系で事前に定義されるcity関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@Form.Native("city")
	@Form.Parameters(min = 2, max = 3)
	private static final class $City extends Form {
		public Object apply(Cons args, Eval eval) {
			final String base = eval.text(args.car());
			final String code = eval.text(args.get(1));
			final City city = City.forCode(base, code);
			if(city == null) return null;
			if(args.size() == 2) return city.getFullName();
			int v = eval.real(args.get(2)).intValueExact();
			return city.getFullPath().get(v);
		}
	}
}
