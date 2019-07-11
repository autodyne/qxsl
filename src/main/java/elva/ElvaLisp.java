/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;
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
public final class ElvaLisp extends AbstractScriptEngine {
	private final Bindings root;

	/**
	 * LISP処理系を構築します。
	 */
	public ElvaLisp() {
		this.root = new Global();
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
	public Bindings createBindings() {
		return new Nested(null, null);
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
		final Nested glob = new Nested(c.getBindings(GLOBAL_SCOPE), root);
		final Nested self = new Nested(c.getBindings(ENGINE_SCOPE), glob);
		final Kernel eval = new Kernel(self);
		try {
			Object last = null;
			for(Object sexp: scan(s)) last = eval.eval(sexp);
			return last;
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
	public final List<Object> scan(Reader r) throws ScriptException {
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
	public final List<Object> scan(String s) throws ScriptException {
		final List<Object> exps = new ArrayList<>();
		try {
			final Parser scan = new Parser(s);
			while(scan.hasNext()) exps.add(scan.next());
			return exps;
		} catch (IOException ex) {
			throw new ScriptException(ex);
		} catch (ElvaLexicalException ex) {
			throw new ScriptException(ex.getMessage());
		}
	}

	/**
	 * LISP処理系で発生する構文の例外を表現します。
	 * この例外はLISP処理系内部でのみ使用されます。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/07/01
	 */
	public static final class ElvaLexicalException extends RuntimeException {
		private static final String TEMP = "lexical error: %s\n%s";
	
		/**
		 * 字句番号と内容を示す文字列を指定して例外を構築します。
		 *
		 * @param message 例外の内容
		 * @param parser 構文解析器
		 */
		public ElvaLexicalException(String message, Parser parser) {
			super(String.format(TEMP, message, parser.getLocal()));
		}
	}
	
	/**
	 * LISP処理系で発生する実行時例外を表現します。
	 * この例外はLISP処理系内部でのみ使用されます。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/07/01
	 */
	public static final class ElvaRuntimeException extends RuntimeException {
		private static final String TEMP = "runtime error: %s\n%%s";
		private final StringJoiner trace;
	
		/**
		 * 問題を示す書式文字列とその引数を指定して例外を構築します。
		 *
		 * @param message 例外の内容
		 * @param args 書式文字列の引数
		 */
		public ElvaRuntimeException(String message, Object...args) {
			super(String.format(TEMP, String.format(message, args)));
			this.trace = new StringJoiner("\n");
		}
	
		/**
		 * 指定された式をこの例外まで辿れる式の追跡履歴に追加します。
		 *
		 * @param sexp 追加する式
		 * @return この例外
		 */
		public final ElvaRuntimeException add(Object sexp) {
			this.trace.add(String.format(" at: '%s'", sexp));
			return this;
		}
	
		/**
		 * この例外を処理系の外部に公開するための変換処理を行います。
		 *
		 * @return 変換された例外
		 */
		public final ScriptException toScriptException() {
			return new ScriptException(String.format(getMessage(), trace));
		}
	}
}
