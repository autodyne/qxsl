/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.script.*;

import static javax.script.ScriptContext.ENGINE_SCOPE;
import static javax.script.ScriptContext.GLOBAL_SCOPE;

/**
 * 無線部開発班が実装するLISP方言「Elva」の{@link ScriptEngine}の実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2019/05/17
 */
public final class ElvaScriptEngine extends AbstractScriptEngine {
	private final Scope root = new Scope(createBindings(), null);

	/**
	 * LISP処理系を構築します。
	 */
	public ElvaScriptEngine() {}

	/**
	 * LISP処理系に関連付けられる新たなファクトリを返します。
	 *
	 * @return ファクトリ
	 */
	public javax.script.ScriptEngineFactory getFactory() {
		return new ElvaScriptEngineFactory();
	}

	/**
	 * 新たに構築した環境で指定された入力からLISPの式を読み取って評価します。
	 * 
	 * @param r 式を読み取るリーダ
	 * @param c 文脈
	 * @return LISPの式を評価した結果
	 * 
	 * @throws ScriptException 読み取り、式の構文上または実行時に発生した例外
	 */
	@Override
	public Object eval(Reader r, ScriptContext c) throws ScriptException {
		try(BufferedReader br = new BufferedReader(r)) {
			return eval(br.lines().collect(Collectors.joining("\n")), c);
		} catch(IOException ex) {
			throw new ScriptException(ex);
		}
	}

	/**
	 * 新たに構築した環境で指定された入力からLISPの式を読み取って評価します。
	 * 
	 * @param s 式
	 * @param c 文脈
	 * @return LISPの最後の式の値
	 * 
	 * @throws ScriptException 式の構文上または実行時に発生した例外
	 */
	@Override
	public Object eval(String s, ScriptContext c) throws ScriptException {
		final Scope glob = new Scope(c.getBindings(GLOBAL_SCOPE), root);
		final Scope self = new Scope(c.getBindings(ENGINE_SCOPE), glob);
		final Lisp eval = new Lisp(self);
		Object last = null;
		for(Object sexp: this.scan(s)) last = eval.eval(sexp);
		return last;
	}

	/**
	 * 指定された入力からLISPの式を読み取ります。
	 * 
	 * @param r 式を読み取るリーダ
	 * @return LISPの式を読み取った結果
	 * 
	 * @throws ScriptException 式の構文上の例外
	 */
	public final List<Object> scan(Reader r) throws ScriptException {
		try(BufferedReader br = new BufferedReader(r)) {
			return scan(br.lines().collect(Collectors.joining("\n")));
		} catch(IOException ex) {
			throw new ScriptException(ex);
		}
	}

	/**
	 * 指定された入力からLISPの式を読み取ります。
	 * 
	 * @param s 式
	 * @return LISPの式を読み取った結果
	 * 
	 * @throws ScriptException 式の構文上の例外
	 */
	public final List<Object> scan(String s) throws ScriptException {
		final ArrayList<Object> exps = new ArrayList<>();
		try {
			final Scanner scan = new Scanner(s);
			while(scan.hasNext()) exps.add(scan.next());
			return exps;
		} catch(IOException ex) {
			throw new ScriptException(ex);
		}
	}

	/**
	 * 指定されたLISPの式の文字列による表現を返します。
	 *
	 * @param sexp 式
	 * @return 文字列による表現
	 *
	 * @since 2019/06/02
	 */
	public static final String toString(Object sexp) {
		if(sexp instanceof String) {
			return String.format("\"%s\"", sexp);
		} else if(sexp instanceof Seq) {
			final Seq list = (Seq) sexp;
			StringJoiner sj = new StringJoiner(" ", "(", ")");
			for(Object elem: list) sj.add(toString(elem));
			return sj.toString();
		} else {
			return String.valueOf(sexp);
		}
	}

	/**
	 * LISP処理系で発生する実行時例外を{@link ScriptException}として生成します。
	 *
	 * @param exp エラーを発生させた式
	 * @param msg エラーの内容を表す文字列の書式
	 * @param arg 書式文字列の引数
	 *
	 * @return 例外
	 */
	private static ScriptException error(Object exp, String msg, Object...arg) {
		final String desc = String.format(msg, arg);
		final String sexp = ElvaScriptEngine.toString(exp);
		return new ScriptException(String.format("error (%s) at %s", desc, sexp));
	}

	/**
	 * LISP処理系で使用される識別子の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Symbol {
		private final String id;
		/**
		 * 名前を指定して識別子を生成します。
		 *
		 * @param id 名前
		 */
		public Symbol(String id) {
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
		 * @return objが{@link Symbol}であり、同じ名前であればtrue
		 */
		public boolean equals(Object obj) {
			if (!(obj instanceof Symbol)) return false;
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
	 * LISP処理系内部で利用される不変リストの実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	public static final class Seq extends AbstractList<Object> {
		/**
		 * 空のリストを利用する際はこのインスタンスを参照します。
		 */
		public static final Seq NIL = new Seq();
		private final List<Object> list;
		/**
		 * 指定された要素を持つリストを構築します。
		 *
		 * @param list 要素
		 */
		public Seq(List<Object> list) {
			this.list = list;
		}
		/**
		 * 指定された要素を持つリストを構築します。
		 *
		 * @param vals 要素
		 */
		public Seq(Object...vals) {
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
		public Seq cdr() {
			return new Seq(list.subList(1, list.size()));
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
	}

	/**
	 * LISP処理系のラムダ式の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Lambda implements Function {
		private final List<Symbol> pars;
		private final Object body;
		private final Lisp lisp;
		/**
		 * 指定された仮引数と式と評価器でラムダ式を生成します。
		 *
		 * @param pars 仮引数
		 * @param body 値の式
		 * @param lisp 評価器
		 */
		public Lambda(Seq pars, Object body, Lisp lisp) {
			this.pars = new ArrayList<>();
			for(Object p: pars) this.pars.add((Symbol) p);
			this.body = body;
			this.lisp = lisp;
		}
		/**
		 * このラムダ式の文字列による表現を返します。
		 *
		 * @return 文字列
		 */
		public String toString() {
			final String str1 = ElvaScriptEngine.toString(pars);
			final String str2 = ElvaScriptEngine.toString(body);
			return String.format("(lambda %s %s)", str1, str2);
		}
		/**
		 * 指定された実引数と評価器でラムダ式の適用を評価します。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Scope env = new Scope(lisp.env);
			if(args.size() == pars.size()) {
				for(int i = 0; i < args.size(); i++) {
					env.put(pars.get(i), eval.eval(args.get(i)));
				}
				return new Lisp(env).eval(body);
			} else throw error(this, "%d arguments required", pars.size());
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
	private static final class Syntax implements Function {
		private final List<Symbol> pars;
		private final Object body;
		private final Lisp lisp;
		/**
		 * 指定された仮引数と式と評価器でラムダ式を生成します。
		 *
		 * @param pars 仮引数
		 * @param body 値の式
		 * @param lisp 評価器
		 */
		public Syntax(Seq pars, Object body, Lisp lisp) {
			this.pars = new ArrayList<>();
			for(Object p: pars) this.pars.add((Symbol) p);
			this.body = body;
			this.lisp = lisp;
		}
		/**
		 * このマクロ式の文字列による表現を返します。
		 *
		 * @return 文字列
		 */
		public String toString() {
			final String str1 = ElvaScriptEngine.toString(pars);
			final String str2 = ElvaScriptEngine.toString(body);
			return String.format("(syntax %s %s)", str1, str2);
		}
		/**
		 * 指定された実引数と評価器でマクロ式の適用を評価します。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Scope env = new Scope(lisp.env);
			if(args.size() == pars.size()) {
				for(int i = 0; i < args.size(); i++) {
					env.put(pars.get(i), args.get(i));
				}
				return eval.eval(new Lisp(env).eval(body));
			} else throw error(this, "%d arguments required", pars.size());
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
		public final Scope outer;
		public final Bindings binds;
		/**
		 * 外側のスコープを指定してスコープを構築します。
		 *
		 * @param outer 外側のスコープ
		 */
		public Scope(Scope outer) {
			this.outer = outer;
			this.binds = new SimpleBindings();
		}
		/**
		 * スコープの内容も指定してスコープを構築します。
		 *
		 * @param outer 外側のスコープ
		 * @param binds スコープの内容
		 */
		public Scope(Bindings binds, Scope outer)  {
			this.outer = outer;
			this.binds = binds;
		}
		/**
		 * 指定された名前に束縛された値を返します。
		 *
		 * @param name 名前
		 * @return 束縛された値
		 * @throws ScriptException 値が見つからない場合
		 */
		public Object get(String name) throws ScriptException {
			if(binds != null && binds.containsKey(name)) {
				return binds.get(name);
			} else if(outer != null) {
				return outer.get(name);
			}
			throw error("variable not declared", name);
		}
		/**
		 * 指定された関数を名前で束縛します。
		 *
		 * @param name 名前
		 * @param func 関数
		 */
		public void put(String name, Function func) {
			binds.put(name, func);
		}
		/**
		 * 指定された値を名前で束縛します。
		 *
		 * @param id 名前
		 * @param target 値
		 */
		public void put(String id, Object target) {
			binds.put(id, target);
		}
		/**
		 * 指定された値を名前で束縛します。
		 *
		 * @param name 名前
		 * @param target 値
		 */
		public void put(Symbol name, Object target) {
			binds.put(name.id, target);
		}
	}

	/**
	 * LISP処理系のスコープ付きの評価器の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	public static final class Lisp {
		public final Scope env;
		/**
		 * 指定されたスコープに対する評価器を構築します。
		 *
		 * @param env 評価器のスコープ
		 */
		private Lisp(Scope env) {
			this.env = env;
		}
		/**
		 * 指定された式の値を求めて{@link Symbol}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		private Symbol name(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof Symbol) return (Symbol) value;
			throw error(sexp, "name required but %s found", value);
		}
		/**
		 * 指定された式の値を求めて{@link Seq}として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Seq list(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof Seq) return (Seq) value;
			throw error(sexp, "list required but %s found", value);
		}
		/**
		 * 指定された式の値を求めて真偽値として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public boolean bool(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof Boolean) return (boolean) value;
			throw error(sexp, "bool required but %s found", value);
		}
		/**
		 * 指定された式の値を求めて実数値として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public BigDecimal real(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof BigDecimal) return (BigDecimal) value;
			throw error(sexp, "real required but %s found", value);
		}
		/**
		 * 指定された式の値を求めて文字列として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public String text(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof String) return (String) value;
			throw error(sexp, "string required but %s found", value);
		}
		/**
		 * 指定された式の値を求めて演算子として返します。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Function func(Object sexp) throws ScriptException {
			final Object value = eval(sexp);
			if(value instanceof Function) return (Function) value;
			throw error(sexp, "function required but %s found", value);
		}
		/**
		 * 指定された式の値を求めます。
		 *
		 * @param sexp 式
		 * @return 返り値
		 * @throws ScriptException 評価により発生した例外
		 */
		public Object eval(Object sexp) throws ScriptException {
			if(sexp instanceof Symbol) return env.get(((Symbol) sexp).id);
			if(sexp instanceof Seq && !Seq.NIL.equals(sexp)) {
				final Seq list = (Seq) sexp;
				Function f = func(list.car());
				this.valid(f, list.cdr());
				return f.apply(list.cdr(), this);
			} else return sexp;
		}
		/**
		 * 引数の個数を検査して必要なら{@link ScriptException}を発生させます。
		 *
		 * @param func 演算子
		 * @param list 実引数
		 * @throws ScriptException 引数の個数が誤っている場合
		 */
		private void valid(Function func, Seq list) throws ScriptException {
			String temp = "%s requires at least %d and at most %d arguments";
			final Arguments args = getClass().getAnnotation(Arguments.class);
			if(args == null) return;
			final int len = list.size();
			final int min = args.min() >= 0? args.min(): Integer.MAX_VALUE;
			final int max = args.max() >= 0? args.max(): Integer.MAX_VALUE;
			final ArrayList<Object> exp = new ArrayList<>();
			exp.add(this);
			exp.addAll(list);
			if(len < min || len > max) throw error(exp, temp, min, max);
		}
	}

	/**
	 * LISP処理系で事前に定義されるクォート識別子です。
	 *
	 * @since 2019/06/07
	 */
	private static final String QUOTE = "quote";

	/**
	 * LISP処理系で事前に定義される準クォート識別子です。
	 *
	 * @since 2019/06/07
	 */
	private static final String QUASI = "quasi";

	/**
	 * LISP処理系で事前に定義されるアンクォート識別子です。
	 *
	 * @since 2019/06/07
	 */
	private static final String UQUOT = "uquot";

	/**
	 * LISP処理系の構文解析器の実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/18
	 */
	private static final class Scanner {
		private final List<String> allTokens;
		private int cursor = 0;

		/**
		 * 指定された式を走査する構文解析器を構築します。
		 *
		 * @param exp 走査対象の式
		 * 
		 * @throws IOException 正規表現の読み込みに失敗した場合
		 */
		public Scanner(String exp) throws IOException {
			this.allTokens = new ArrayList<>();
			final String regex = getRegexPattern();
			Matcher matcher = Pattern.compile(regex).matcher(exp);
			while(matcher.find()) allTokens.add(matcher.group(1));
		}
		/**
		 * リソースファイルからLISPの字句の正規表現を取得します。
		 *
		 * @return 正規表現
		 *
		 * @throws IOException 正規表現の読み込みに失敗した場合
		 */
		public String getRegexPattern() throws IOException {
			final URL path = getClass().getResource("elva.lex");
			Reader r = new InputStreamReader(path.openStream());
			try (BufferedReader reader = new BufferedReader(r)) {
				return reader.lines().collect(Collectors.joining());
			}
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
			if(atom.equals("(")) return nextList();
			if(atom.matches("\".*\"")) return escape(atom);
			if(atom.equals("'")) return new Seq(new Symbol(QUOTE), next());
			if(atom.equals("`")) return new Seq(new Symbol(QUASI), next());
			if(atom.equals(",")) return new Seq(new Symbol(UQUOT), next());
			if(atom.equals(")")) throw new ScriptException("isolated ')'");
			return asSymbolOrReal(atom);
		}
		/**
		 * 指定された文字列のエスケープ処理を行います。
		 *
		 * @param text 文字列
		 * @return 処理された文字列
		 */
		private final String escape(String text) {
			text = text.substring(1, text.length() - 1);
			text = text.replace("\\t", "\t");
			text = text.replace("\\b", "\b");
			text = text.replace("\\n", "\n");
			text = text.replace("\\r", "\r");
			text = text.replace("\\f", "\f");
			text = text.replace("\\\"", "\"");
			text = text.replace("\\\\", "\\");
			return text;
		}
		/**
		 * 指定されたアトムをシンボルまたは実数として返します。
		 *
		 * @return 実数値
		 */
		private final Object asSymbolOrReal(String atom) {
			try {
				if(atom.contains(".")) return new BigDecimal(atom);
				return BigDecimal.valueOf(+Integer.parseInt(atom));
			} catch (NumberFormatException ex) {
				return new Symbol(atom);
			}
		}
		/**
		 * 先頭の括弧が読まれた状態で以降のリスト式を返します。
		 *
		 * @return 次のリスト式
		 * @throws ScriptException 構文に問題がある場合
		 */
		private final Seq nextList() throws ScriptException {
			final ArrayList<Object> list = new ArrayList<>();
			boolean closed = false;
			loop: while(cursor < allTokens.size()) {
				switch(allTokens.get(cursor++)) {
					case ")": closed = true; break loop;
					default: --cursor; list.add(next());
				}
			}
			if(closed) return list.isEmpty()? Seq.NIL: new Seq(list);
			else throw new ScriptException("unclosed parenthesis '('");
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
	@Arguments(min = 1, max = 1)
	private static final class $Quote implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
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
	@Arguments(min = 1, max = 1)
	private static final class $Quasi implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			if(!(args.car() instanceof Seq)) return args.car();
			return this.map((Seq) args.car(), eval);
		}
		private Seq map(Seq source, Lisp eval) throws ScriptException {
			final ArrayList<Object> target = new ArrayList<>();
			for(Object obj: source) try {
				final Seq list = (Seq) obj;
				if(list.car() instanceof Symbol) {
					switch(list.car().toString()) {
						case UQUOT: target.add(eval.eval(obj)); continue;
						case QUASI: target.add(obj); continue;
					}
				}
				target.add(map(list, eval));
			} catch (ClassCastException ex) {
				target.add(obj);
			} catch (IndexOutOfBoundsException ex) {
				target.add(obj);
			}
			return new Seq(target);
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
	@Arguments(min = 1, max = 1)
	private static final class $Uquot implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
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
	@Arguments(min = 1, max = -1)
	private static final class $Progn implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			Object last = null;
			for(Object v: args) last = eval.eval(v);
			return last;
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
	@Arguments(min = 2, max = 2)
	private static final class $Set implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
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
	@Arguments(min = 0, max = -1)
	private static final class $List implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			ArrayList<Object> arguments = new ArrayList<>();
			for(Object v: args) arguments.add(eval.eval(v));
			return new Seq(arguments);
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
	@Arguments(min = 1, max = 1)
	private static final class $Car implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
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
	@Arguments(min = 1, max = 1)
	private static final class $Cdr implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			return eval.list(args.car()).cdr();
		}
	}

	/**
	 * LISP処理系で事前に定義されるempty?関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/17
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Empty$ implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			return eval.list(args.car()).isEmpty();
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
	@Arguments(min = 1, max = 1)
	private static final class $Length implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			return BigDecimal.valueOf(eval.list(args.car()).size());
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
	@Arguments(min = 2, max = 2)
	private static final class $Member implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object val = eval.eval(args.car());
			final Seq list = eval.list(args.get(1));
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
	@Arguments(min = 2, max = 2)
	private static final class $MapCar implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final ArrayList<Object> target = new ArrayList<>();
			final Function op = eval.func(args.car());
			final Seq source = eval.list(args.get(1));
			for(Object e: source) target.add(op.apply(new Seq(e), eval));
			return new Seq(target);
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
	@Arguments(min = 2, max = 2)
	private static final class $Equal implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
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
	@Arguments(min = 3, max = 3)
	private static final class $If implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			return eval.eval(args.get(eval.bool(args.car())? 1:2));
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
	@Arguments(min = 2, max = -1)
	private static final class $And implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
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
	@Arguments(min = 2, max = -1)
	private static final class $Or implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			for(Object v: args) if(eval.bool(v)) return true;
			return false;
		}
	}

	/**
	 * LISP処理系で事前に定義されるand関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 1, max = 1)
	private static final class $Not implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			return !eval.bool(args.car());
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
	@Arguments(min = 2, max = -1)
	private static final class $Add implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.add(eval.real(v));
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
	@Arguments(min = 2, max = -1)
	private static final class $Sub implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.subtract(eval.real(v));
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
	@Arguments(min = 2, max = -1)
	private static final class $Mul implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.multiply(eval.real(v));
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
	@Arguments(min = 2, max = -1)
	private static final class $Div implements Function {
		private final int MODE = BigDecimal.ROUND_FLOOR;
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.divide(eval.real(v), MODE);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義される剰余演算子です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Arguments(min = 2, max = -1)
	private static final class $Mod implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.remainder(eval.real(v));
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
	@Arguments(min = 2, max = -1)
	private static final class $Lt implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final List<BigDecimal> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.real(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i).compareTo(vals.get(i + 1)) >= 0) return false;
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
	@Arguments(min = 2, max = -1)
	private static final class $Gt implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final ArrayList<BigDecimal> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.real(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i).compareTo(vals.get(i + 1)) <= 0) return false;
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
	@Arguments(min = 2, max = -1)
	private static final class $Le implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final ArrayList<BigDecimal> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.real(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i).compareTo(vals.get(i + 1)) > 0) return false;
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
	@Arguments(min = 2, max = -1)
	private static final class $Ge implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final ArrayList<BigDecimal> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.real(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i).compareTo(vals.get(i + 1)) < 0) return false;
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
	@Arguments(min = 1, max = 1)
	private static final class $StrHead implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
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
	@Arguments(min = 1, max = 1)
	private static final class $StrTail implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			return eval.text(args.car()).substring(1);
		}
	}

	/**
	 * LISP処理系で事前に定義されるmatch関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/16
	 */
	@Arguments(min = 2, max = 2)
	private static final class $Match implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			return eval.text(args.get(1)).matches(eval.text(args.car()));
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
	@Arguments(min = 2, max = 2)
	private static final class $Lambda implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object p = args.get(0), body = args.get(1);
			Seq pars = p instanceof Seq? (Seq) p: new Seq(p);
			if(!pars.stream().allMatch(Symbol.class::isInstance)) {
				final ArrayList<Object> exp = new ArrayList<>();
				exp.add(this);
				exp.addAll(args);
				throw error(exp, "all parameters must be names");
			} else return new Lambda(pars, body, eval);
		}
	}

	/**
	 * LISP処理系で事前に定義されるsyntax関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Arguments(min = 2, max = 2)
	private static final class $Syntax implements Function {
		public Object apply(Seq args, Lisp eval) throws ScriptException {
			final Object p = args.get(0), body = args.get(1);
			Seq pars = p instanceof Seq? (Seq) p: new Seq(p);
			if(!pars.stream().allMatch(Symbol.class::isInstance)) {
				final ArrayList<Object> exp = new ArrayList<>();
				exp.add(this);
				exp.addAll(args);
				throw error(exp, "all parameters must be names");
			} else return new Syntax(pars, body, eval);
		}
	}

	/**
	 * LISP処理系で事前に定義された関数や値を保持する環境を作成します。
	 *
	 * @return 事前に定義された環境
	 */
	@Override
	public Bindings createBindings() {
		final Scope lude = new Scope(null);
		lude.put("nil", Seq.NIL);
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
		lude.put(QUOTE, new $Quote());
		lude.put(QUASI, new $Quasi());
		lude.put(UQUOT, new $Uquot());

		/*
		 * basic functions for sequential processing
		 *
		 * (progn statements)
		 */
		lude.put("progn", new $Progn());

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
		lude.put("list",   new $List());
		lude.put("car",    new $Car());
		lude.put("cdr",    new $Cdr());
		lude.put("empty?", new $Empty$());
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

		/*
		 * basic functions for logical operation
		 *
		 * (and expressions)
		 * (or  expressions)
		 * (not expression)
		 */
		lude.put("and", new $And());
		lude.put("or",  new $Or());
		lude.put("not", new $Not());

		/*
		 * basic functions for arithmetical operation
		 *
		 * (+ expressions)
		 * (- expressions)
		 * (* expressions)
		 * (/ expressions)
		 * (% expressions)
		 */
		lude.put("+",   new $Add());
		lude.put("-",   new $Sub());
		lude.put("*",   new $Mul());
		lude.put("/",   new $Div());
		lude.put("mod", new $Mod());

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
		 * basic functions for regex matching
		 *
		 * (match pattern string)
		 */
		lude.put("match", new $Match());

		/*
		 * basic functions for lambda & syntax(macro) generation
		 *
		 * (lambda (parameters) value)
		 * (syntax (parameters) macro)
		 */
		lude.put("lambda", new $Lambda());
		lude.put("syntax", new $Syntax());
		return lude.binds;
	}
}
