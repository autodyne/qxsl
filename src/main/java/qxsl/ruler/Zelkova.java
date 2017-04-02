/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import java.io.BufferedReader;
import java.io.Reader;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.script.*;
import qxsl.field.*;
import qxsl.model.Exch;
import qxsl.model.Item;
import static javax.script.ScriptContext.*;

/**
 * コンテストの規約をLISPで記述するための{@link ScriptEngine}の実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/27
 */
public final class Zelkova extends javax.script.AbstractScriptEngine {
	private final Scope galaxy = new Scope(this.createBindings(), null);

	/**
	 * LISP処理系を構築します。
	 */
	public Zelkova() {}

	/**
	 * LISP処理系のファクトリは未定義で、このメソッドはnullを返します。
	 *
	 * @return null
	 */
	public javax.script.ScriptEngineFactory getFactory() {
		return null;
	}

	/**
	 * 新たに構築した環境で指定された文字列をLISPの式として評価します。
	 * 
	 * @param r 式を読み取るリーダ
	 * @param c 文脈
	 */
	@Override
	public Object eval(Reader r, ScriptContext c) throws ScriptException {
		try(BufferedReader br = new BufferedReader(r)) {
			return eval(br.lines().collect(Collectors.joining()), c);
		} catch(java.io.IOException ex) {
			throw new ScriptException(ex);
		}
	}

	/**
	 * 新たに構築した環境で指定された文字列をLISPの式として評価します。
	 * 
	 * @param s 式
	 * @param c 文脈
	 */
	@Override
	public Object eval(String s, ScriptContext c) throws ScriptException {
		final Scope global = new Scope(c.getBindings(GLOBAL_SCOPE), galaxy);
		final Scope engine = new Scope(c.getBindings(ENGINE_SCOPE), global);
		try {
			Object last = null;
			final Scanner scan = new Scanner(s);
			final Runtime eval = new Runtime(engine);
			while(scan.hasNext()) last = eval.eval(scan.next());
			return last;
		} catch(Exception ex) {
			throw new ScriptException(ex);
		}
	}

	/**
	 * 識別子の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Name implements java.io.Serializable {
		private final String id;
		/**
		 * 名前を指定して識別子を生成します。
		 *
		 * @param id 名前
		 */
		public Name(String id) {
			this.id = id;
		}
		/**
		 * この識別子の名前を返します。
		 *
		 * @return 名前
		 */
		public String toString() {
			return id;
		}
		/**
		 * この識別子が指定された値と等価であればtrueを返します。
		 *
		 * @param obj 比較対象のオブジェクト
		 * @return objが{@link Name}であり、同じ名前であればtrue
		 */
		public boolean equals(Object obj) {
			if (!(obj instanceof Name)) return false;
			return obj.toString().equals(toString());
		}
		/**
		 * この識別子のハッシュ値を返します。
		 *
		 * @return ハッシュ値
		 */
		public int hashCode() {
			return id.hashCode();
		}
	}

	/**
	 * 処理系内部で利用されるリストの実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class List extends java.util.AbstractList<Object> {
		public static final List NIL = new List(Arrays.asList());
		private final java.util.List<Object> list;
		/**
		 * 指定された要素を持つリストを構築します。
		 *
		 * @param list 要素
		 */
		public List(java.util.List<Object> list) {
			this.list = list;
		}
		/**
		 * 指定された要素を持つリストを構築します。
		 *
		 * @param name 名前
		 * @param next 要素
		 */
		public List(String name, Object next) {
			this(Arrays.asList(new Name(name), next));
		}
		/**
		 * 指定された要素を評価してリストを構築します。
		 *
		 * @param list 要素
		 * @param eval 評価器
		 * @throws Exception 評価により発生した例外
		 */
		public List(List list, Runtime eval) throws Exception {
			ArrayList<Object> res = new ArrayList<>();
			for(Object v: list) res.add(eval.eval(v));
			this.list = res;
		}
		/**
		 * このリストの先頭のコンスセルのCAR部を返します。
		 *
		 * @return CAR部
		 */
		public Object car() {
			return list.get(0);
		}
		/**
		 * このリストの先頭のコンスセルのCDR部を返します。
		 *
		 * @return CDR部
		 */
		public List cdr() {
			return new List(list.subList(1, list.size()));
		}
		/**
		 * このリストの指定された位置の要素を返します。
		 *
		 * @param index 要素の位置
		 * @return 要素
		 */
		public Object get(int index) {
			return list.get(index);
		}
		/**
		 * このリストの要素数を返します。
		 *
		 * @return 要素数
		 */
		public int size() {
			return list.size();
		}
		/**
		 * このリストを関数適用の式として評価します。
		 *
		 * @param eval 評価を移譲する評価器
		 * @return 評価により得た返り値
		 * @throws Exception 評価により発生した例外
		 */
		public Object apply(Runtime eval) throws Exception {
			if(this.equals(NIL)) return this;
			Object car = eval.eval(this.car());
			if(car instanceof Lambda) return ((Lambda) car).apply(cdr(), eval);
			if(car instanceof Syntax) return ((Syntax) car).apply(cdr(), eval);
			if(car instanceof Native) return ((Native) car).apply(cdr(), eval);
			throw new Exception(String.format("%s uncallable: %s", car, this));
		}
		/**
		 * このリストの文字列による表現を返します。
		 *
		 * @return 文字列
		 */
		public String toString() {
			StringJoiner sj = new StringJoiner(" ", "(", ")");
			for(Object elem:this) sj.add(Runtime.print(elem));
			return sj.toString();
		}
	}

	/**
	 * LISP処理系のシステム関数が継承するインターフェースです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static interface Native {
		/**
		 * 指定された実引数と評価器に対し、返り値を求めます。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 * @throws Exception 評価により生じた例外
		 */
		public Object apply(List args, Runtime eval) throws Exception;
	}

	/**
	 * LISP処理系のラムダ式の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Lambda implements java.io.Serializable {
		private final List params;
		private final Object body;
		private final Runtime run;
		/**
		 * 指定された仮引数と式と評価器でラムダ式を生成します。
		 *
		 * @param def ((仮引数) 式)
		 * @param run 評価器
		 * @throws Exception 引数defが文法に従わない場合
		 */
		public Lambda(List def, Runtime run) throws Exception {
			try {
				this.params = (List) def.car();
				this.body = def.get(1);
				this.run = run;
			} catch(ClassCastException ex) {
				throw new Exception("invalid definition: " + this);
			} catch(IndexOutOfBoundsException ex) {
				throw new Exception("invalid definition: " + this);
			}
			for(Object par: params) if(!(par instanceof Name)) {
				throw new Exception("invalid definition: " + this);
			}
		}
		/**
		 * このラムダ式の文字列による表現を返します。
		 *
		 * @return 文字列
		 */
		public String toString() {
			return String.format("(lambda %s %s)", params, body);
		}
		/**
		 * 指定された実引数と評価器でラムダ式の適用を評価します。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public Object apply(List args, Runtime eval) throws Exception {
			final Scope env = new Scope(run.env);
			if(args.size() == params.size()) {
				for(int i = 0; i < args.size(); i++) {
					env.put((Name) params.get(i), eval.eval(args.get(i)));
				}
				return new Runtime(env).eval(body);
			}
			throw new Exception(this + " needs " + params.size() + "args");
		}
	}

	/**
	 * LISP処理系のマクロ式の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Syntax implements java.io.Serializable {
		private final List params;
		private final Object body;
		private final Runtime run;
		/**
		 * 指定された仮引数と式と評価器でマクロ式を生成します。
		 *
		 * @param def ((仮引数) 式)
		 * @param run 評価器
		 * @throws Exception 引数defが文法に従わない場合
		 */
		public Syntax(List def, Runtime run) throws Exception {
			try {
				this.params = (List) def.car();
				this.body = def.get(1);
				this.run = run;
			} catch(ClassCastException ex) {
				throw new Exception("invalid definition: " + this);
			} catch(IndexOutOfBoundsException ex) {
				throw new Exception("invalid definition: " + this);
			}
			for(Object par: params) if(!(par instanceof Name)) {
				throw new Exception("invalid definition: " + this);
			}
		}
		/**
		 * このマクロ式の文字列による表現を返します。
		 *
		 * @return 文字列
		 */
		public String toString() {
			return String.format("(syntax %s %s)", params, body);
		}
		/**
		 * 指定された実引数と評価器でマクロ式の適用を評価します。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public Object apply(List args, Runtime eval) throws Exception {
			final Scope env = new Scope(run.env);
			if(args.size() == params.size()) {
				for(int i = 0; i < args.size(); i++) {
					env.put((Name) params.get(i), args.get(i));
				}
				return eval.eval(new Runtime(env).eval(body));
			}
			throw new Exception(this + " needs " + params.size() + "args");
		}
	}

	/**
	 * LISP処理系内部におけるコンテスト定義の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/20
	 */
	private static final class Contest extends qxsl.ruler.Contest {
		private final String name;
		private final java.util.List<qxsl.ruler.Section> sections;
		/**
		 * 指定された名前と部門集合で部門を構築します。
		 *
		 * @param args (名前 (部門1 部門2 ...))
		 * @param eval 評価器
		 * @throws Exception 評価により発生した例外
		 */
		public Contest(List args, Runtime eval) throws Exception {
			this.name = (String) eval.eval(args.get(0));
			ArrayList<Section> list = new ArrayList<>();
			for(Object v: args.subList(1, args.size())) {
				list.add((Section) eval.eval(v));
			}
			this.sections = Collections.unmodifiableList(list);
		}
		@Override
		public String getName() {
			return name;
		}
		@Override
		public java.util.Iterator<qxsl.ruler.Section> iterator() {
			return sections.iterator();
		}
	}

	/**
	 * LISP処理系内部における同時に参加不可能な部門の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/20
	 */
	private static final class Section extends qxsl.ruler.Section {
		private final String name;
		private final Lambda rule;
		private final Runtime run;
		/**
		 * 指定された名前と規約で部門を構築します。
		 *
		 * @param args (名前 規約)
		 * @param eval 評価器
		 * @throws Exception 評価により発生した例外
		 */
		public Section(List args, Runtime eval) throws Exception {
			this.name = (String) eval.eval(args.get(0));
			this.rule = (Lambda) eval.eval(args.get(1));
			this.run = eval;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public Message validate(Item item) throws Exception {
			Object res = run.eval(new List(Arrays.asList(rule, item)));
			if(res instanceof String) return new Failure((String) res);
			try {
				final Object callKey = ((List) res).get(0);
				final Object multKey = ((List) res).get(1);
				final int score = (int) ((List) res).get(2);
				return new Success(callKey, multKey, score);
			} catch (NullPointerException ex) {
				return new Failure("lambda must return (call-key mult-key score)");
			} catch (ClassCastException ex) {
				return new Failure("lambda must return (call-key mult-key score)");
			} catch (IndexOutOfBoundsException ex) {
				return new Failure("lambda must return (call-key mult-key score)");
			}
		}
	}

	/**
	 * LISP処理系の識別子を捕捉する静的スコープの実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Scope {
		public final Scope enclosure;
		public final Bindings bind;
		/**
		 * 外側のスコープを指定してスコープを構築します。
		 *
		 * @param enclosure 外側のスコープ
		 */
		public Scope(Scope enclosure) {
			this.enclosure = enclosure;
			this.bind = new SimpleBindings();
		}
		/**
		 * スコープの内容も指定してスコープを構築します。
		 *
		 * @param enclosure 外側のスコープ
		 * @param bind スコープの内容
		 */
		public Scope(Bindings bind, Scope enclosure)  {
			this.enclosure = enclosure;
			this.bind = bind;
		}
		/**
		 * 指定された名前に束縛された値を返します。
		 *
		 * @param name 名前
		 * @return 束縛された値
		 * @throws Exception 値が見つからない場合
		 */
		public Object get(String name) throws Exception {
			if(bind != null && bind.containsKey(name)) {
				return bind.get(name);
			} else if(enclosure != null) {
				return enclosure.get(name);
			}
			throw new Exception("'" + name + "' not declared");
		}
		/**
		 * 指定された関数を名前で束縛します。
		 *
		 * @param name 名前
		 * @param func 関数
		 */
		public void put(String name, Native func) {
			bind.put(name, func);
		}
		/**
		 * 指定された値を名前で束縛します。
		 *
		 * @param id 名前
		 * @param target 値
		 */
		public void put(String id, Object target) {
			bind.put(id, target);
		}
		/**
		 * 指定された値を名前で束縛します。
		 *
		 * @param name 名前
		 * @param target 値
		 */
		public void put(Name name, Object target) {
			bind.put(name.id, target);
		}
	}

	/**
	 * LISP処理系の評価器並びにプリンタの実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Runtime implements java.io.Serializable {
		public final Scope env;
		/**
		 * 指定されたスコープに対する評価器を構築します。
		 *
		 * @param env 評価器のスコープ
		 */
		public Runtime(Scope env) {
			this.env = env;
		}
		/**
		 * 指定された式の文字列による表現を返します。
		 *
		 * @param sexp 式
		 * @return 文字列による表現
		 */
		public static final String print(Object sexp) {
			if(sexp instanceof String) {
				return String.format("\"%s\"", sexp);
			} else {
				return String.valueOf(sexp);
			}
		}
		/**
		 * 指定された式の値を求めて{@link Name}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public Name name(Object sexp) throws Exception {
			final Object value = eval(sexp);
			if(value instanceof Name) return (Name) value;
			throw new Exception(sexp + " must be a name");
		}
		/**
		 * 指定された式の値を求めて{@link List}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public List list(Object sexp) throws Exception {
			final Object value = eval(sexp);
			if(value instanceof List) return (List) value;
			throw new Exception(sexp + " must be a list");
		}
		/**
		 * 指定された式の値を求めて真偽値として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public boolean bool(Object sexp) throws Exception {
			final Object bb = eval(sexp);
			if(bb instanceof Boolean) return (boolean) bb;
			throw new Exception(sexp + " must be a bool");
		}
		/**
		 * 指定された式の値を求めて整数値として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public int integral(Object sexp) throws Exception {
			final Object bb = eval(sexp);
			if(bb instanceof Integer) return (Integer) bb;
			throw new Exception(sexp + " must be an integer");
		}
		
		/**
		 * 指定された式の値を求めて文字列として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public String text(Object sexp) throws Exception {
			final Object text = eval(sexp);
			if(text instanceof String) return (String) text;
			throw new Exception(sexp + " must be a string");
		}
		/**
		 * 指定された式の値を求めて{@link Exch}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public Exch<?> exch(Object sexp) throws Exception {
			final Object value = eval(sexp);
			if(value instanceof Exch) return (Exch) value;
			throw new Exception(sexp + " must be a exch");
		}
		/**
		 * 指定された式の値を求めて{@link Item}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public Item item(Object sexp) throws Exception {
			final Object value = eval(sexp);
			if(value instanceof Item) return (Item) value;
			throw new Exception(sexp + " must be a item");
		}
		/**
		 * 指定された式の値を求めます。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws Exception 評価により発生した例外
		 */
		public Object eval(Object sexp) throws Exception {
			if(sexp instanceof Name) return env.get(((Name) sexp).id);
			if(sexp instanceof List) return ((List) sexp).apply(this);
			return sexp;
		}
	}

	/**
	 * LISP処理系の構文解析器の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Scanner {
		private final ArrayList<String> allTokens;
		private int cursor = 0;
		/**
		 * 指定された式を走査する構文解析器を構築します。
		 *
		 * @param sexp 走査対象の式
		 */
		public Scanner(String sexp) {
			final String lex = "['\\(\\)]|\".*?\"|[^\\s\\(\\)]+";
			Matcher matcher = Pattern.compile(lex).matcher(sexp);
			allTokens = new ArrayList<>();
			while(matcher.find()) allTokens.add(matcher.group());
		}
		/**
		 * 次の式がある場合はtrueを返します。
		 *
		 * @return 次の式がある場合
		 */
		public final boolean hasNext() {
			return cursor < allTokens.size();
		}
		/**
		 * 次の式を返します。
		 *
		 * @return 次の式
		 * @throws Exception 構文に問題がある場合
		 */
		public final Object next() throws Exception {
			final String atom = allTokens.get(cursor++);
			if(atom.equals(")")) throw new Exception("where's (?");
			if(atom.equals("(")) {
				final ArrayList<Object> list = new ArrayList<>();
				boolean closed = false;
				loop: while(cursor < allTokens.size()) {
					switch(allTokens.get(cursor++)) {
						case ")": closed = true; break loop;
						default: --cursor; list.add(next());
					}
				}
				if(!closed) throw new Exception("where's )?");
				return list.isEmpty()? List.NIL: new List(list);
			}
			if(atom.matches("\\d+")) return Integer.parseInt(atom);
			if(atom.equals("\'")) return new List("quote", next());
			if(!atom.matches("\".*?\"")) return new Name(atom);
			else return atom.substring(1, atom.length() - 1);
		}
	}

	/**
	 * LISP処理系で事前に定義されるprogn関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	private static final class $Progn implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			Object last = null;
			for(Object v: args) last = eval.eval(v);
			return last;
		}
	}

	/**
	 * LISP処理系で事前に定義されるprint関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	private static final class $Print implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			System.out.println(eval.print(eval.eval(args.car())));
			return null;
		}
	}

	/**
	 * LISP処理系で事前に定義されるset関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	private static final class $Set implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			final Object value = eval.eval(args.get(1));
			eval.env.put(eval.name(args.get(0)), value);
			return value;
		}
	}

	/**
	 * LISP処理系で事前に定義されるmember関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	private static final class $Member implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			final Object val = eval.eval(args.car());
			final List list = eval.list(args.get(1));
			return list.contains(val);
		}
	}

	/**
	 * LISP処理系で事前に定義されるequal関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	private static final class $Equal implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			final Object l = eval.eval(args.get(0));
			final Object r = eval.eval(args.get(1));
			return l == null? r == null: l.equals(r);
		}
	}

	/**
	 * LISP処理系で事前に定義されるif関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	private static final class $If implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			return eval.eval(args.get(eval.bool(args.car())? 1:2));
		}
	}

	/**
	 * LISP処理系で事前に定義されるcond関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	private static final class $Cond implements Native {
		private final $Progn progn = new $Progn();
		public Object apply(List args, Runtime eval) throws Exception {
			ArrayList<List> stmts = new ArrayList<>();
			for(Object st: args) stmts.add((List) st);
			for(List st: stmts) if(eval.bool(st.car())) {
				return progn.apply(st.cdr(), eval);
			}
			return null;
		}
	}

	/**
	 * LISP処理系で事前に定義されるand関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	private static final class $And implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			for(Object v: args) if(!eval.bool(v)) return false;
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義されるor関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	private static final class $Or implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			for(Object v: args) if(eval.bool(v)) return true;
			return false;
		}
	}

	/**
	 * LISP処理系で事前に定義される加算演算子です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	private static final class $Add implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			if(args.isEmpty()) throw new Exception("empty argument (+)");
			int val = eval.integral(args.car());
			for(Object v: args.cdr()) val += eval.integral(v);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義される減算演算子です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	private static final class $Sub implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			if(args.isEmpty()) throw new Exception("empty argument (-)");
			int val = eval.integral(args.car());
			for(Object v: args.cdr()) val -= eval.integral(v);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義される乗算演算子です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	private static final class $Mul implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			if(args.isEmpty()) throw new Exception("empty argument (*)");
			int val = eval.integral(args.car());
			for(Object v: args.cdr()) val *= eval.integral(v);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義される除算演算子です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	private static final class $Div implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			if(args.isEmpty()) throw new Exception("empty argument (/)");
			int val = eval.integral(args.car());
			for(Object v: args.cdr()) val /= eval.integral(v);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義される不等号&lt;の関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/17
	 */
	private static final class $Lt implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			if(args.isEmpty()) throw new Exception("empty argument (<)");
			final ArrayList<Integer> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.integral(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i) >= vals.get(i + 1)) return false;
			}
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義される不等号&gt;の関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/17
	 */
	private static final class $Gt implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			if(args.isEmpty()) throw new Exception("empty argument (>)");
			final ArrayList<Integer> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.integral(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i) <= vals.get(i + 1)) return false;
			}
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義される不等号&lt;=の関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/17
	 */
	private static final class $Le implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			if(args.isEmpty()) throw new Exception("empty argument (<=)");
			final ArrayList<Integer> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.integral(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i) > vals.get(i + 1)) return false;
			}
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義される不等号&gt;=の関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/17
	 */
	private static final class $Ge implements Native {
		public Object apply(List args, Runtime eval) throws Exception {
			if(args.isEmpty()) throw new Exception("empty argument (>=)");
			final ArrayList<Integer> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.integral(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i) < vals.get(i + 1)) return false;
			}
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義された関数や値を保持する環境を作成します。
	 *
	 * @return 事前に定義された環境
	 */
	@Override
	public javax.script.Bindings createBindings() {
		final Scope lude = new Scope(null);
		lude.put("null", null);
		lude.put("true", true);
		lude.put("false", false);

		/*
		 * basic functions for syntax operation
		 *
		 * (quote expression)
		 */
		lude.put("quote", (args, eval) -> args.car());

		/*
		 * basic functions for sequential processing
		 *
		 * (progn statements)
		 */
		lude.put("progn", new $Progn());

		/*
		 * basic functions for standard output
		 *
		 * (print expression)
		 */
		lude.put("print", new $Print());

		/*
		 * basic functions for variable assignment
		 *
		 * (set symbol-expression expression)
		 */
		lude.put("set", new $Set());

		/*
		 * basic functions for list operation
		 * 
		 * (list elements)
		 * (car list)
		 * (cdr list)
		 * (member value list)
		 */
		lude.put("list", (args, eval) -> new List(args, eval));
		lude.put("car",  (args, eval) -> eval.list(args.car()).car());
		lude.put("cdr",  (args, eval) -> eval.list(args.car()).cdr());
		lude.put("member", new $Member());

		/*
		 * basic functions for checking equality
		 *
		 * (equal expression expression)
		 */
		lude.put("equal", new $Equal());

		/*
		 * conditional operators
		 *
		 * (if condition then else)
		 * (cond (condition statements)*)
		 */
		lude.put("if",   new $If());
		lude.put("cond", new $Cond());

		/*
		 * basic functions for logical operation
		 *
		 * (and expressions)
		 * (or  expressions)
		 * (not expression)
		 */
		lude.put("and", new $And());
		lude.put("or",  new $Or());
		lude.put("not", (args, eval) -> !eval.bool(args.car()));

		/*
		 * basic functions for arithmetical operation
		 *
		 * (+ expressions)
		 * (- expressions)
		 * (* expressions)
		 * (/ expressions)
		 */
		lude.put("+", new $Add());
		lude.put("-", new $Sub());
		lude.put("*", new $Mul());
		lude.put("/", new $Div());

		/*
		 * basic functions for numerical comparison
		 *
		 * (<  expressions)
		 * (>  expressions)
		 * (<= expressions)
		 * (>= expressions)
		 */
		lude.put("<",  new $Lt());
		lude.put(">",  new $Gt());
		lude.put("<=", new $Le());
		lude.put(">=", new $Ge());

		/*
		 * basic functions for string triming
		 *
		 * (head string-expression)
		 * (tail string-expression)
		 */
		lude.put("head", (args, eval) -> eval.text(args.car()).substring(0, 1));
		lude.put("tail", (args, eval) -> eval.text(args.car()).substring(1));

		/*
		 * basic functions for lambda & syntax(macro) generation
		 *
		 * (lambda (parameters) value)
		 * (syntax (parameters) macro)
		 */
		lude.put("lambda", (args, eval) -> new Lambda(args, eval));
		lude.put("syntax", (args, eval) -> new Syntax(args, eval));

		/*
		 * preinstalled functions for contest & section definition
		 * 
		 * (contest symbol-expression sections)
		 * (special symbol-expression lambda)
		 */
		lude.put("contest", (args, eval) -> new Contest(args, eval));
		lude.put("section", (args, eval) -> new Section(args, eval));

		/*
		 * preinstalled functions for rcvd & sent access
		 *
		 * (rcvd item-expression)
		 * (sent item-expression)
		 */
		lude.put("rcvd", (args, eval) -> eval.item(args.car()).getRcvd());
		lude.put("sent", (args, eval) -> eval.item(args.car()).getSent());

		/*
		 * preinstalled functions for field access
		 *
		 * (hour item-expression)
		 * (call item-expression)
		 * (band item-expression)
		 * (mode item-expression)
		 */
		lude.put("hour", (args, eval) -> eval.item(args.car()).get(Time.class).hour());
		lude.put("call", (args, eval) -> eval.item(args.car()).value(Call.class));
		lude.put("band", (args, eval) -> eval.item(args.car()).value(Band.class));
		lude.put("mode", (args, eval) -> eval.item(args.car()).value(Mode.class));

		/*
		 * preinstalled functions for rcvd / sent field access
		 *
		 * (rstq exch-expression)
		 * (code exch-expression)
		 *
		 * @since 2016/11/25
		 */
		lude.put("rstq", (args, eval) -> eval.exch(args.car()).value(RSTQ.class));
		lude.put("code", (args, eval) -> eval.exch(args.car()).value(Code.class));

		/*
		 * preinstalled functions for city / pref access
		 *
		 * (city JCC/JCG-code)
		 * (pref JCC/JCG-code)
		 */
		lude.put("city", (args, eval) -> new City(eval.text(args.car())).getCityName());
		lude.put("pref", (args, eval) -> new City(eval.text(args.car())).getPrefName());
		return lude.bind;
	}
}
