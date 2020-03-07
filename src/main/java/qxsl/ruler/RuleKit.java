/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import javax.script.Bindings;
import javax.script.ScriptException;
import javax.xml.namespace.QName;

import elva.*;

import qxsl.extra.field.City;
import qxsl.extra.field.Time;
import qxsl.model.Exch;
import qxsl.model.Item;
import qxsl.model.Tuple;

import static elva.Elva.ElvaRuntimeException;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.time.temporal.TemporalAdjusters.dayOfWeekInMonth;

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

	/**
	 * LISP処理系を構築します。
	 */
	public RuleKit() {
		this.elva = new Elva();
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
		return (Handler) this.elva.eval(reader, createBindings());
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
		return (Contest) this.elva.eval(reader, createBindings());
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
		private final Form rule;
		private final Eval eval;

		/**
		 * 指定された規約定義と評価器で部門を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public HandlerImpl(Cons rule, Eval eval) {
			this.name = eval.text(rule.get(0));
			this.rule = eval.form(rule.get(1));
			this.eval = eval;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public Item apply(Item item) {
			return eval.apply(rule, item).as(Item.class);
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
		private final List<Section> list;
		private final Form rule;
		private final Eval eval;
		private Form starting = null;
		private Form deadLine = null;

		/**
		 * 指定された規約定義と評価器で規約を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public ContestImpl(Cons rule, Eval eval) {
			super(eval.text(rule.get(0)));
			this.list = new ArrayList<>();
			this.rule = eval.form(rule.get(1));
			this.eval = eval;
		}

		@Override
		public LocalDate getStartingDate(int year) {
			if(starting == null) return null;
			return eval.apply(starting, year).as(LocalDate.class);
		}

		@Override
		public LocalDate getDeadLineDate(int year) {
			if(deadLine == null) return null;
			return eval.apply(deadLine, year).as(LocalDate.class);
		}

		@Override
		public Iterator<Section> iterator() {
			return this.list.iterator();
		}

		@Override
		public int score(Summary sum) {
			if (sum.accepted().isEmpty()) return 0;
			final var args = new ArrayList<Sexp>();
			for(var s: Cons.wrap(rule, sum.score())) args.add(s);
			for(var mult: sum.mults()) args.add(Cons.wrap(mult));
			return this.eval.sInt(Cons.cons(args));
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
		private final Form rule;
		private final Eval eval;

		/**
		 * 指定された規約定義と評価器で部門を構築します。
		 *
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public SectionImpl(Cons rule, Eval eval) {
			super(eval.text(rule.get(1)), eval.text(rule.get(2)));
			this.rule = eval.form(rule.get(3));
			this.eval = eval;
		}

		@Override
		public Message apply(Item item) {
			return eval.apply(rule, item).as(Message.class);
		}
	}

	/**
	 * LISP処理系で事前に定義された関数や値を保持する環境を作成します。
	 *
	 * @return 事前に定義された環境
	 */
	private final Bindings createBindings() {
		final Nest env = new Nest(null, null);
		/*
		 * load script
		 *
		 * (load file)
		 */
		env.put(new $Load());

		/*
		 * define handler, contest, or section
		 *
		 * (handler name lambda)
		 * (contest name scoring)
		 * (section contest name code validation)
		 */
		env.put(new $Handler());
		env.put(new $Contest());
		env.put(new $Section());

		/*
		 * schedule contest
		 *
		 * (starting contest date)
		 * (deadline contest date)
		 */
		env.put(new $Starting());
		env.put(new $DeadLine());

		/*
		 * create success or failure
		 *
		 * (success ITEM score keys...)
		 * (failure ITEM message)
		 */
		env.put(new $Success());
		env.put(new $Failure());

		/*
		 * create item
		 *
		 * (item)
		 */
		env.put(new $Item());

		/*
		 * get rcvd and sent
		 *
		 * (rcvd item)
		 * (sent item)
		 */
		env.put(new $Rcvd());
		env.put(new $Sent());

		/*
		 * field access
		 *
		 * (get-field item namespace name)
		 * (set-field item namespace name value-string)
		 */
		env.put(new $GetField());
		env.put(new $SetField());

		/*
		 * time access
		 *
		 * (hour item)
		 */
		env.put(new $Hour());

		/*
		 * city access
		 *
		 * (city database-name code region-level)
		 */
		env.put(new $City());

		/*
		 * date calculation
		 *
		 * (date year month day-of-week number)
		 */
		env.put(new $Date());
		return env;
	}

	/**
	 * この関数はクラスパスからプログラムを読み込みます。
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
				for(var s: elva.scan(isr)) value = eval.apply(s);
				return value;
			} catch (IOException | ScriptException ex) {
				final String msg = "failed in loading %s: %s";
				throw new ElvaRuntimeException(msg, name, ex);
			}
		}
	}

	/**
	 * この関数は交信記録に対する手続きを生成します。
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
	 * この関数はコンテストの規約の実体を生成します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/15
	 */
	@Form.Native("contest")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Contest extends Form {
		public Object apply(Cons args, Eval eval) {
			return new ContestImpl(args, eval);
		}
	}

	/**
	 * この関数はコンテストの部門を規約に追加します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/15
	 */
	@Form.Native("section")
	@Form.Parameters(min = 4, max = 4)
	private static final class $Section extends Form {
		public Object apply(Cons args, Eval eval) {
			final var test = eval.apply(args.car());
			final var sect = new SectionImpl(args, eval);
			test.as(ContestImpl.class).list.add(sect);
			return test;
		}
	}

	/**
	 * この関数はコンテストの開始日を規約に追加します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/07
	 */
	@Form.Native("starting")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Starting extends Form {
		public Object apply(Cons args, Eval eval) {
			final var test = eval.apply(args.car());
			final var date = eval.form(args.get(1));
			test.as(ContestImpl.class).starting = date;
			return test;
		}
	}

	/**
	 * この関数はコンテストの締切日を規約に追加します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/07
	 */
	@Form.Native("deadline")
	@Form.Parameters(min = 2, max = 2)
	private static final class $DeadLine extends Form {
		public Object apply(Cons args, Eval eval) {
			final var test = eval.apply(args.car());
			final var date = eval.form(args.get(1));
			test.as(ContestImpl.class).deadLine = date;
			return test;
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
	@Form.Native("success")
	@Form.Parameters(min = 3, max = -1)
	private static final class $Success extends Form {
		public Object apply(Cons args, Eval eval) {
			final var arg1 = eval.apply(args.car());
			final int arg2 = eval.sInt(args.get(1));
			final var arg3 = args.cdr(2).map(eval);
			final var keys = arg3.stream().toArray();
			final var item = arg1.as(Item.class);
			return new Success(item, arg2, keys);
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
	@Form.Native("failure")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Failure extends Form {
		public Object apply(Cons args, Eval eval) {
			final var item = eval.apply(args.car());
			final var text = eval.text(args.get(1));
			return new Failure(item.as(Item.class), text);
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
	@Form.Native("item")
	@Form.Parameters(min = 0, max = 0)
	private static final class $Item extends Form {
		public Object apply(Cons args, Eval eval) {
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
	@Form.Native("rcvd")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Rcvd extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.apply(args.car()).as(Item.class).getRcvd();
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
	@Form.Native("sent")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Sent extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.apply(args.car()).as(Item.class).getSent();
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
	@Form.Native("get-field")
	@Form.Parameters(min = 3, max = 3)
	private static final class $GetField extends Form {
		public Object apply(Cons args, Eval eval) {
			final var tuple = eval.apply(args.car());
			final var space = eval.text(args.get(1));
			final var local = eval.text(args.get(2));
			final var qname = new QName(space, local);
			return tuple.as(Tuple.class).value(qname);
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
	@Form.Native("set-field")
	@Form.Parameters(min = 4, max = 4)
	private static final class $SetField extends Form {
		public Object apply(Cons args, Eval eval) {
			final var tuple = eval.apply(args.get(0));
			final var space = eval.text(args.get(1));
			final var local = eval.text(args.get(2));
			final var value = eval.text(args.get(3));
			final var qname = new QName(space, local);
			return tuple.as(Tuple.class).set(qname, value);
		}
	}

	/**
	 * この関数は指定された時間帯で時刻を取り出します。
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
			final var time = eval.apply(args.get(0));
			final var text = eval.text(args.get(1));
			final var date = time.as(ZonedDateTime.class);
			ZoneId zone = ZoneId.of(text, ZoneId.SHORT_IDS);
			return date.withZoneSameInstant(zone).getHour();
		}
	}

	/**
	 * この関数は指定された符号の地域名を取り出します。
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

	/**
	 * この関数は指定された月と曜日の日付を計算します。
	 * 定期開催されるコンテストの日付計算に使用します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/07
	 */
	@Form.Native("date")
	@Form.Parameters(min = 4, max = 4)
	private static final class $Date extends Form {
		public Object apply(Cons args, Eval eval) {
			final int y = eval.sInt(args.get(0));
			final int m = eval.sInt(args.get(1));
			final int w = eval.sInt(args.get(2));
			final int n = eval.sInt(args.get(3));
			final var a = DayOfWeek.of(w);
			final var b = dayOfWeekInMonth(n, a);
			return LocalDate.of(y, m, 1).with(b);
		}
	}
}
