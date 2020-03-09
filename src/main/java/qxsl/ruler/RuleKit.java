/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.io.Reader;
import java.io.StringReader;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.TemporalAdjusters;
import javax.script.Bindings;
import javax.script.ScriptException;
import javax.xml.namespace.QName;

import elva.Cons;
import elva.Elva;
import elva.Eval;
import elva.Form;
import elva.Nest;
import elva.Sexp;

import qxsl.extra.field.City;
import qxsl.extra.field.Time;
import qxsl.model.Exch;
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
 * @see Elva 内部で使用されるLISP処理系
 */
public final class RuleKit {
	/**
	 * LISP処理系を構築します。
	 */
	public RuleKit() {}

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
		return (Handler) new Elva().eval(reader, createBindings());
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
		return (Contest) new Elva().eval(reader, createBindings());
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
		final var sexp = String.format("(load \"%s\")", name);
		return handler(new StringReader(sexp));
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
		final var sexp = String.format("(load \"%s\")", name);
		return contest(new StringReader(sexp));
	}

	/**
	 * LISP処理系が内蔵する関数や値を参照する環境を返します。
	 *
	 * @return 事前に定義された環境
	 */
	private final Bindings createBindings() {
		final Nest env = new Nest(null);

		/*
		 * define handler, contest, or section
		 *
		 * (handler name lambda)
		 * (contest name scoring)
		 * (section contest name code validation)
		 */
		env.put(new Handler.$Handler());
		env.put(new Contest.$Contest());
		env.put(new Section.$Section());

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
			final Item item = eval.apply(args.get(0)).value(Item.class);
			final int score = eval.apply(args.get(1)).ival();
			final var mults = args.cdr(2).map(eval).stream();
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
	@Form.Native("failure")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Failure extends Form {
		public Object apply(Cons args, Eval eval) {
			final Item item = eval.apply(args.get(0)).value(Item.class);
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
			return eval.apply(args.car()).value(Item.class).getRcvd();
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
			return eval.apply(args.car()).value(Item.class).getSent();
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
			final var space = eval.apply(args.get(1)).text();
			final var local = eval.apply(args.get(2)).text();
			final var qname = new QName(space, local);
			return tuple.value(Tuple.class).value(qname);
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
			final var tuple = eval.apply(args.car());
			final var space = eval.apply(args.get(1)).text();
			final var local = eval.apply(args.get(2)).text();
			final var value = eval.apply(args.get(3)).text();
			final var qname = new QName(space, local);
			return tuple.value(Tuple.class).set(qname, value);
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
			final var head = eval.apply(args.get(0));
			final var name = eval.apply(args.get(1)).text();
			final var time = head.value(ZonedDateTime.class);
			ZoneId zone = ZoneId.of(name, ZoneId.SHORT_IDS);
			return time.withZoneSameInstant(zone).getHour();
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
			final var base = eval.apply(args.get(0)).text();
			final var code = eval.apply(args.get(1)).text();
			final var city = City.forCode(base, code);
			if(city == null) return null;
			if(args.size() == 2) return city.getFullName();
			final int idx = eval.apply(args.get(2)).ival();
			return city.getFullPath().get(idx);
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
			final int y = eval.apply(args.get(0)).ival();
			final var m = eval.apply(args.get(1)).text();
			final var w = eval.apply(args.get(2)).text();
			final int n = eval.apply(args.get(3)).ival();
			final var week = DayOfWeek.valueOf(w);
			var adj = TemporalAdjusters.dayOfWeekInMonth(n, week);
			return LocalDate.of(y, Month.valueOf(m), 1).with(adj);
		}
	}
}
