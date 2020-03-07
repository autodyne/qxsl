/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.*;

import static javax.script.ScriptContext.ENGINE_SCOPE;
import static javax.script.ScriptContext.GLOBAL_SCOPE;

/**
 * 無線部開発班が実装するLISP方言「Elva」の{@link ScriptEngine}の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/17
 */
public final class Elva extends AbstractScriptEngine {
	private final Bindings root;

	/**
	 * LISP処理系を構築します。
	 */
	public Elva() {
		this.root = new Root();
	}

	/**
	 * LISP処理系に関連付けられる新たなファクトリを返します。
	 *
	 * @return null
	 */
	public ScriptEngineFactory getFactory() {
		return null;
	}

	/**
	 * LISP処理系で事前に定義された関数や値を保持する環境を作成します。
	 *
	 * @return 環境
	 */
	public final Bindings createBindings() {
		return new Nest(null, null);
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
		} catch (IOException ex) {
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
		final var glob = new Nest(c.getBindings(GLOBAL_SCOPE), root);
		final var self = new Nest(c.getBindings(ENGINE_SCOPE), glob);
		final var eval = new Eval(self);
		try {
			final Stream<Sexp> vals = scan(s).stream().map(eval);
			return vals.reduce((h, t) -> t).orElse(null).value();
		} catch (ElvaRuntimeException ex) {
			throw ex.toScriptException();
		}
	}

	/**
	 * 指定された入力からLISPの式を読み取ります。
	 *
	 * @param r 式を読み取るリーダ
	 * @return LISPの式を読み取った結果
	 *
	 * @throws ScriptException 式の構文上の例外
	 */
	public final List<Sexp> scan(Reader r) throws ScriptException {
		try(BufferedReader br = new BufferedReader(r)) {
			return scan(br.lines().collect(Collectors.joining("\n")));
		} catch (IOException ex) {
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
	public final List<Sexp> scan(String s) throws ScriptException {
		final List<Sexp> exps = new ArrayList<>();
		try {
			final var scan = new Scanner(s);
			while(scan.hasNext()) exps.add(scan.next());
			return exps;
		} catch (IOException ex) {
			throw new ScriptException(ex);
		} catch (ElvaLexicalException ex) {
			throw new ScriptException(ex.getMessage());
		}
	}

	/**
	 * LISP処理系で使用される構文解析器の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/18
	 */
	private static final class Scanner implements Iterator<Sexp> {
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
			final var path = getClass().getResource("elva.lex");
			final var r = new InputStreamReader(path.openStream());
			try (BufferedReader reader = new BufferedReader(r)) {
				return reader.lines().collect(Collectors.joining());
			}
		}

		/**
		 * 次の式がある場合はtrueを返します。
		 *
		 * @return 次の式がある場合
		 */
		@Override
		public final boolean hasNext() {
			return cursor < allTokens.size();
		}

		/**
		 * 現在の位置の直前の字句をまとめた文字列を返します。
		 *
		 * @return 現在位置の直前の文字列
		 */
		public final String getLocal() {
			final int sindex = Math.max(cursor - 10, 0);
			List<String> strm = allTokens.subList(sindex, cursor);
			return strm.stream().collect(Collectors.joining(" "));
		}

		/**
		 * 次の式を返します。
		 *
		 * @return 次の式
		 * @throws ElvaLexicalException 構文に問題がある場合
		 */
		@Override
		public final Sexp next() throws ElvaLexicalException {
			final String atom = allTokens.get(cursor++);
			if(atom.equals("(")) return nextList();
			if(atom.matches("\".*\"")) return new Atom(escape(atom));
			if(atom.equals("'"))  return Symbol.Quote.QUOTE.quote(next());
			if(atom.equals("`"))  return Symbol.Quote.QUASI.quote(next());
			if(atom.equals(","))  return Symbol.Quote.UQUOT.quote(next());
			if(atom.equals(",@")) return Symbol.Quote.UQSPL.quote(next());
			if(!atom.equals(")")) return Sexp.wrap(asSymbolOrReal(atom));
			throw new ElvaLexicalException("isolated ')'", this);
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
		 * 指定されたアトムを実数またはシンボルとして返します。
		 *
		 * @param atom アトム式
		 * @return 実数値または名前
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
		 * @throws ElvaLexicalException 構文に問題がある場合
		 */
		private final Cons nextList() throws ElvaLexicalException {
			final var list = new ArrayList<Sexp>();
			boolean closed = false;
			loop: while(cursor < allTokens.size()) {
				switch(allTokens.get(cursor++)) {
					case ")": closed = true; break loop;
					default: --cursor; list.add(next());
				}
			}
			if(closed) return Cons.cons(list);
			throw new ElvaLexicalException("isolated '('", this);
		}
	}

	/**
	 * LISP処理系で発生する構文の例外を表現します。
	 * この例外はLISP処理系内部でのみ使用されます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/01
	 */
	public static final class ElvaLexicalException extends RuntimeException {
		private static final String TEMP = "lexical error: %s\n%s";

		/**
		 * 字句番号と内容を示す文字列を指定して例外を構築します。
		 *
		 * @param message 例外の内容
		 * @param scanner 構文解析器
		 */
		public ElvaLexicalException(String message, Scanner scanner) {
			super(String.format(TEMP, message, scanner.getLocal()));
		}
	}

	/**
	 * LISP処理系で発生する実行時例外を表現します。
	 * この例外はLISP処理系内部でのみ使用されます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/01
	 */
	public static final class ElvaRuntimeException extends RuntimeException {
		private static final String TEMP = "runtime error: %s\n%%s";
		private final StringJoiner trace;
		private final String error;

		/**
		 * 問題を示す書式文字列とその引数を指定して例外を構築します。
		 *
		 * @param message 例外の内容
		 * @param args 書式文字列の引数
		 */
		public ElvaRuntimeException(String message, Object...args) {
			this.error = String.format(TEMP, String.format(message, args));
			this.trace = new StringJoiner("\n");
		}

		/**
		 * 指定された例外を包む例外を構築します。
		 *
		 * @param ex 例外
		 */
		public ElvaRuntimeException(Exception ex) {
			this(ex.toString());
		}

		/**
		 * 指定された式をこの例外まで辿れる式の追跡履歴に追加します。
		 *
		 * @param sexp 追加する式
		 * @return この例外
		 */
		public final ElvaRuntimeException add(Sexp sexp) {
			this.trace.add(String.format(" at: '%s'", sexp));
			return this;
		}

		/**
		 * この例外を処理系の外部に公開するための文字列を生成します。
		 *
		 * @return 例外の内容を示す文字列
		 */
		@Override
		public final String getMessage() {
			return String.format(error, trace);
		}

		/**
		 * この例外を処理系の外部に公開するための変換処理を行います。
		 *
		 * @return 変換された例外
		 */
		public final ScriptException toScriptException() {
			return new ScriptException(this.getMessage());
		}
	}
}
