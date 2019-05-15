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
	 * LISP処理系で発生する意味論上のエラーを例外として生成します。
	 *
	 * @param msg エラーの内容を表す文字列
	 * @param exp エラーを発生させた式
	 *
	 * @return 例外
	 */
	private static ScriptException error(String msg, Object exp) {
		return new ScriptException(String.format("%s: %s", msg, exp));
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
	public static final class List extends java.util.AbstractList<Object> {
		public static final List NIL = new List();
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
		 * @param vals 要素
		 */
		public List(Object...vals) {
			this(Arrays.asList(vals));
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
	 * LISP処理系の関数やマクロはこのインターフェースを実装します。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/15
	 */
	private static interface Operator extends java.io.Serializable {
		/**
		 * 指定された実引数と評価器に対し、返り値を求めます。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 * @throws ScriptException 評価により生じた例外
		 */
		public Object apply(List args, Runtime eval) throws ScriptException;
	}

	/**
	 * LISP処理系のシステム関数が継承するインターフェースです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static interface Native extends Operator {}

	/**
	 * LISP処理系のシステム関数のうち特にエラー検証を実施する関数です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/15
	 */
	private static abstract class NativeWithErrorValidation implements Native {
		/**
		 * 指定された引数に対するこの関数の実行時エラーを発生させます。
		 *
		 * @param msg エラーの内容
		 * @param arg エラーを生じさせた引数
		 *
		 * @return 例外
		 */
		private ScriptException error(String msg, List args) {
			final ArrayList<Object> list = new ArrayList<>();
			list.add(new Name(getClass().getSimpleName()));
			list.addAll(args);
			return error(msg, new List(list));
		}
	}

	/**
	 * LISP処理系のラムダ式の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Lambda implements Operator {
		private final List params;
		private final Object body;
		private final Runtime run;
		/**
		 * 指定された仮引数と式と評価器でラムダ式を生成します。
		 *
		 * @param params 仮引数
		 * @param body 値の式
		 * @param run 評価器
		 */
		public Lambda(List params, Object body, Runtime run) {
			this.params = params;
			this.body = body;
			this.run = run;
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
		 * @throws ScriptException 評価により発生した例外
		 */
		public Object apply(List args, Runtime eval) throws ScriptException {
			final Scope env = new Scope(run.env);
			if(args.size() == params.size()) {
				for(int i = 0; i < args.size(); i++) {
					env.put((Name) params.get(i), eval.eval(args.get(i)));
				}
				return new Runtime(env).eval(body);
			}
			throw error(String.format("%d arguments required", params.size()), this);
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
	private static final class Syntax implements Operator {
		private final List params;
		private final Object body;
		private final Runtime run;
		/**
		 * 指定された仮引数と式と評価器でラムダ式を生成します。
		 *
		 * @param params 仮引数
		 * @param body 値の式
		 * @param run 評価器
		 */
		public Syntax(List params, Object body, Runtime run) {
			this.params = params;
			this.body = body;
			this.run = run;
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
		 * @throws ScriptException 評価により発生した例外
		 */
		public Object apply(List args, Runtime eval) throws ScriptException {
			final Scope env = new Scope(run.env);
			if(args.size() == params.size()) {
				for(int i = 0; i < args.size(); i++) {
					env.put((Name) params.get(i), args.get(i));
				}
				return eval.eval(new Runtime(env).eval(body));
			}
			throw error(String.format("%d arguments required", params.size()), this);
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
	private static final class ContestImpl extends qxsl.ruler.Contest {
		private final String name;
		private final java.util.List<Section> sections;
		/**
		 * 指定された識別名と部門集合で部門を構築します。
		 *
		 * @param ident このコンテストの識別名
		 * @param sects このコンテストの部門
		 */
		public ContestImpl(String ident, java.util.List<Section> sects) {
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
	 * LISP処理系内部における同時に参加不可能な部門の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/20
	 */
	private static final class SectionImpl extends Section {
		private final String name;
		private final Runtime eval;
		private final Operator rule;
		/**
		 * 指定された名前と規約で部門を構築します。
		 *
		 * @param name 部門名
		 * @param rule 規約
		 * @param eval 評価器
		 */
		public SectionImpl(String name, Operator rule, Runtime eval) {
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
			Object list = eval.eval(new List(Arrays.asList(rule, item)));
			if(list instanceof String) return new Failure((String) list);
			try {
				final Object callKey = ((List) list).get(0);
				final Object multKey = ((List) list).get(1);
				final int score = (int) ((List) list).get(2);
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
		 * @throws ScriptException 値が見つからない場合
		 */
		public Object get(String name) throws ScriptException {
			if(bind != null && bind.containsKey(name)) {
				return bind.get(name);
			} else if(enclosure != null) {
				return enclosure.get(name);
			}
			throw error("variable not declared", name);
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
		 * @throws ScriptException 評価により発生した例外
		 */
		public Name name(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof Name) return (Name) value;
			throw error("type error: a name is required", sexp);
		}
		/**
		 * 指定された式の値を求めて{@link List}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public List list(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof List) return (List) value;
			throw error("type error: a list is required", sexp);
		}
		/**
		 * 指定された式の値を求めて真偽値として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public boolean bool(Object sexp) throws ScriptException {
			final Object bb = eval(sexp);
			if(bb instanceof Boolean) return (boolean) bb;
			throw error("type error: a bool is required", sexp);
		}
		/**
		 * 指定された式の値を求めて整数値として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public int integer(Object sexp) throws ScriptException {
			final Object bb = eval(sexp);
			if(bb instanceof Integer) return (Integer) bb;
			throw error("type error: an int is required", sexp);
		}
		/**
		 * 指定された式の値を求めて文字列として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public String text(Object sexp) throws ScriptException {
			final Object text = eval(sexp);
			if(text == null) return null;
			if(text instanceof String) return (String) text;
			throw error("type error: a string is required", sexp);
		}
		/**
		 * 指定された式の値を求めて演算子として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Operator oprt(Object sexp) throws ScriptException {
			final Object oprt = eval(sexp);
			if(oprt == null) return null;
			if(oprt instanceof Operator) return (Operator) oprt;
			throw error("type error: an operator is required", sexp);
		}
		/**
		 * 指定された式の値を求めて{@link Exch}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Exch<?> exch(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof Exch) return (Exch) value;
			throw error("type error: an Exch is required", sexp);
		}
		/**
		 * 指定された式の値を求めて{@link Item}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Item item(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof Item) return (Item) value;
			throw error("type error: an Item is required", sexp);
		}
		/**
		 * 指定された式の値を求めます。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Object eval(Object sexp) throws ScriptException {
			if(sexp instanceof Name) return env.get(((Name) sexp).id);
			if(sexp instanceof List) {
				final List list = (List) sexp;
				if(List.NIL.equals(sexp)) return sexp;
				return oprt(list.car()).apply(list.cdr(), this);
			} else {
				return sexp;
			}
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
		private final String sym = "['`,\\(\\)]";
		private final String val = "[^'`,\\(\\)\"\\s]+";
		private final String str = "\"([^\\\\]|\\\\\")*?\"";
		private static final Name QUOTE = new Name("quote");
		private static final Name QUASI = new Name("quasi");
		private static final Name UQUOT = new Name("uquot");
		private int cursor = 0;

		/**
		 * 指定された式を走査する構文解析器を構築します。
		 *
		 * @param sexp 走査対象の式
		 */
		public Scanner(String sexp) {
			this.allTokens = new ArrayList<>();
			final String lex = String.join("|", sym, val, str);
			Matcher matcher = Pattern.compile(lex).matcher(sexp);
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
		 * @throws ScriptException 構文に問題がある場合
		 */
		public final Object next() throws ScriptException {
			final String atom = allTokens.get(cursor++);
			if(atom.equals(")")) throw new ScriptException("where's (?");
			if(atom.equals("(")) {
				final ArrayList<Object> list = new ArrayList<>();
				boolean closed = false;
				loop: while(cursor < allTokens.size()) {
					switch(allTokens.get(cursor++)) {
						case ")": closed = true; break loop;
						default: --cursor; list.add(next());
					}
				}
				if(!closed) throw new ScriptException("where's )?");
				return list.isEmpty()? List.NIL: new List(list);
			}
			if(atom.matches("\\d+")) return Integer.parseInt(atom);
			if(atom.equals("'")) return new List(QUOTE, next());
			if(atom.equals("`")) return new List(QUASI, next());
			if(atom.equals(",")) return new List(UQUOT, next());
			if(!atom.matches("\".*?\"")) return new Name(atom);
			else return atom.substring(1, atom.length() - 1);
		}
	}

	/**
	 * LISP処理系で事前に定義されるクォート関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $Quote extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
			return args.car();
		}
	}

	/**
	 * LISP処理系で事前に定義される準クォート関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $Quasi extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
			if(!(args.car() instanceof List)) return args.car();
			return this.map((List) args.car(), eval);
		}
		private List map(List source, Runtime eval) throws ScriptException {
			final ArrayList<Object> target = new ArrayList<>();
			for(Object obj: source) try {
				final List list = (List) obj;
				if(Scanner.UQUOT.equals(list.car())) target.add(eval.eval(obj));
				else target.add(map(list, eval));
			} catch (ClassCastException ex) {
				target.add(obj);
			} catch (IndexOutOfBoundsException ex) {
				target.add(obj);
			}
			return new List(target);
		}
	}

	/**
	 * LISP処理系で事前に定義されるアンクォート関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $Uquot extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
			return eval.eval(args.car());
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
	private static final class $Progn extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
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
	private static final class $Print extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
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
	private static final class $Set extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 2) throw error("two arguments required", args);
			final Object value = eval.eval(args.get(1));
			eval.env.put(eval.name(args.get(0)), value);
			return value;
		}
	}

	/**
	 * LISP処理系で事前に定義されるlist関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $List extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			ArrayList<Object> arguments = new ArrayList<>();
			for(Object v: args) arguments.add(eval.eval(v));
			return new List(arguments);
		}
	}

	/**
	 * LISP処理系で事前に定義されるcar関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $Car extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
			return eval.list(args.car()).car();
		}
	}

	/**
	 * LISP処理系で事前に定義されるcdr関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $Cdr extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
			return eval.list(args.car()).cdr();
		}
	}

	/**
	 * LISP処理系で事前に定義されるlength関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $Length extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
			return eval.list(args.car()).size();
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
	private static final class $Member extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 2) throw error("two arguments required", args);
			final Object val = eval.eval(args.car());
			final List list = eval.list(args.get(1));
			return list.contains(val);
		}
	}

	/**
	 * LISP処理系で事前に定義されるmapcar関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $MapCar extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 2) throw error("two arguments required", args);
			final ArrayList<Object> target = new ArrayList<>();
			final Operator op = eval.oprt(args.car());
			final List source = eval.list(args.get(1));
			for(Object e: source) target.add(op.apply(new List(e), eval));
			return new List(target);
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
	private static final class $Equal extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
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
	private static final class $If extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
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
	private static final class $Cond extends NativeWithErrorValidation {
		private final $Progn progn = new $Progn();
		public Object apply(List args, Runtime eval) throws ScriptException {
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
	private static final class $And extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
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
	private static final class $Or extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
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
	private static final class $Add extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("at least one argument required", args);
			int val = eval.integer(args.car());
			for(Object v: args.cdr()) val += eval.integer(v);
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
	private static final class $Sub extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("at least one argument required", args);
			int val = eval.integer(args.car());
			for(Object v: args.cdr()) val -= eval.integer(v);
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
	private static final class $Mul extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("at least one argument required", args);
			int val = eval.integer(args.car());
			for(Object v: args.cdr()) val *= eval.integer(v);
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
	private static final class $Div extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("at least one argument required", args);
			int val = eval.integer(args.car());
			for(Object v: args.cdr()) val /= eval.integer(v);
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
	private static final class $Lt extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("at least one argument required", args);
			final ArrayList<Integer> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.integer(v));
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
	private static final class $Gt extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("at least one argument required", args);
			final ArrayList<Integer> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.integer(v));
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
	private static final class $Le extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("at least one argument required", args);
			final ArrayList<Integer> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.integer(v));
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
	private static final class $Ge extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("at least one argument required", args);
			final ArrayList<Integer> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.integer(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i) < vals.get(i + 1)) return false;
			}
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義されるstr-head関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $StrHead extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
			return eval.text(args.car()).substring(0, 1);
		}
	}

	/**
	 * LISP処理系で事前に定義されるstr-tail関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $StrTail extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 1) throw error("a single argument required", args);
			return eval.text(args.car()).substring(1);
		}
	}

	/**
	 * LISP処理系で事前に定義されるlambda関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $Lambda extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 2) throw error("make sure (syntax param body)", args);
			final Object par = args.car();
			final List param = par instanceof List? (List) par: new List(par);
			if(!param.stream().allMatch(p -> p instanceof Name)) {
				throw error("all parameter must be names", args);
			}
			return new Lambda(param, args.get(1), eval);
		}
	}

	/**
	 * LISP処理系で事前に定義されるlambda関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	private static final class $Syntax extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 2) throw error("make sure (syntax param body)", args);
			final Object par = args.car();
			final List param = par instanceof List? (List) par: new List(par);
			if(!param.stream().allMatch(p -> p instanceof Name)) {
				throw error("all parameter must be names", args);
			}
			return new Syntax(param, args.get(1), eval);
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
	private static final class $Contest extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.isEmpty()) throw error("make sure (contest name sections)", args);
			final String ident = eval.text(args.car());
			ArrayList<Section> list = new ArrayList<>();
			for(Object v: args.subList(1, args.size())) try {
				list.add((Section) eval.eval(v));
			} catch (ClassCastException ex) {
				throw error("make sure (contest name SECTIONS)", args);
			}
			return new ContestImpl(ident, list);
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
	private static final class $Section extends NativeWithErrorValidation {
		public Object apply(List args, Runtime eval) throws ScriptException {
			if(args.size() != 2) throw error("make sure (section name (lambda))", args);
			return new SectionImpl(eval.text(args.car()), eval.oprt(args.get(1)), eval);
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
		lude.put("nil", List.NIL);
		lude.put("null", null);
		lude.put("true", true);
		lude.put("false", false);

		/*
		 * basic functions for syntax operation
		 *
		 * (quote expression)
		 * (quasi expression)
		 * (uquot expression)
		 */
		lude.put(Scanner.QUOTE.toString(), new $Quote());
		lude.put(Scanner.QUASI.toString(), new $Quasi());
		lude.put(Scanner.UQUOT.toString(), new $Uquot());

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
		 * (length list)
		 * (member value list)
		 */
		lude.put("list", new $List());
		lude.put("car", new $Car());
		lude.put("cdr", new $Cdr());
		lude.put("length", new $Length());
		lude.put("member", new $Member());

		/*
		 * basic functions for mapping operation
		 * 
		 * (mapcar sequence)
		 */
		lude.put("mapcar", new $MapCar());

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
		 * (str-head string-expression)
		 * (str-tail string-expression)
		 */
		lude.put("str-head", new $StrHead());
		lude.put("str-tail", new $StrTail());

		/*
		 * basic functions for lambda & syntax(macro) generation
		 *
		 * (lambda (parameters) value)
		 * (syntax (parameters) macro)
		 */
		lude.put("lambda", new $Lambda());
		lude.put("syntax", new $Syntax());

		/*
		 * preinstalled functions for contest & section definition
		 * 
		 * (contest symbol-expression sections)
		 * (special symbol-expression lambda)
		 */
		lude.put("contest", new $Contest());
		lude.put("section", new $Section());

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
