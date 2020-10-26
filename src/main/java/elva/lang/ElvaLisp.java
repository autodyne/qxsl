/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.io.*;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.regex.Pattern;
import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngineFactory;

import elva.warn.ElvaLexicalException;
import elva.warn.ElvaRuntimeException;

import static elva.lang.NameNode.Quote.QUASI;
import static elva.lang.NameNode.Quote.QUOTE;
import static elva.lang.NameNode.Quote.UQSPL;
import static elva.lang.NameNode.Quote.UQUOT;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.stream.Collectors.joining;
import static javax.script.ScriptContext.ENGINE_SCOPE;
import static javax.script.ScriptContext.GLOBAL_SCOPE;

/**
 * 無線部開発班が実装するLISP処理系の対外的な実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/17
 */
public final class ElvaLisp extends AbstractScriptEngine {
	private static final String PATH = "elva.lex";
	private final ScopeMap lisp;

	/**
	 * LISP処理系を構築します。
	 */
	public ElvaLisp() {
		this(ElvaLisp.class.getClassLoader());
	}

	/**
	 * LISP処理系を構築します。
	 *
	 *
	 * @param loader 関数を供給するクラスローダ
	 */
	public ElvaLisp(ClassLoader loader) {
		this.lisp = new ScopeMap(loader);
		lisp.put("null", null);
		lisp.put("nil",  NodeBase.NIL);
		lisp.put(BoolNode.T);
		lisp.put(BoolNode.F);
		ServiceLoader.load(FormBase.class).forEach(lisp::put);
	}

	/**
	 * LISP処理系に関連付けられる新たなファクトリを返します。
	 *
	 *
	 * @return ファクトリ
	 */
	@Override
	public ScriptEngineFactory getFactory() {
		return new ElvaInfo();
	}

	/**
	 * LISP処理系が内蔵する関数や値を参照する環境を返します。
	 *
	 *
	 * @return 環境
	 */
	@Override
	public final Bindings createBindings() {
		return lisp.fork();
	}

	/**
	 * 指定された入力から式を読み取ります。
	 *
	 *
	 * @param reader 式を読み取るリーダ
	 *
	 * @return 式を読み取った結果
	 *
	 * @throws UncheckedIOException 式の読み取りの例外
	 * @throws ElvaLexicalException 式の構文面での例外
	 */
	public static final ListBase scan(Reader reader) {
		try(final var br = new BufferedReader(reader)) {
			return scan(br.lines().collect(joining("\n")));
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * 指定された入力から式を読み取ります。
	 *
	 *
	 * @param source 式
	 *
	 * @return 式を読み取った結果
	 *
	 * @throws UncheckedIOException 式の読み取りの例外
	 * @throws ElvaLexicalException 式の構文面での例外
	 */
	public static final ListBase scan(String source) {
		try {
			final var scan = new Lexical(source);
			final var list = new LinkedList<NodeBase>();
			while(scan.hasNext()) list.add(scan.next());
			return new CoverSeq(list);
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * 新たに構築した環境で指定された入力から式を読み取って評価します。
	 *
	 *
	 * @param r 式を読み取るリーダ
	 * @param c 文脈
	 *
	 * @return 式を評価した結果
	 *
	 * @throws UncheckedIOException 式の読み取りの例外
	 * @throws ElvaLexicalException 式の構文面での例外
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	public Object eval(Reader r, ScriptContext c) {
		final var root = lisp.fork().merge(c.getBindings(GLOBAL_SCOPE));
		final var self = root.fork().merge(c.getBindings(ENGINE_SCOPE));
		return scan(r).map(new ElvaEval(self)).last().value();
	}

	/**
	 * 新たに構築した環境で指定された入力から式を読み取って評価します。
	 *
	 *
	 * @param s 式
	 * @param c 文脈
	 *
	 * @return LISPの最後の式の値
	 *
	 * @throws UncheckedIOException 式の読み取りの例外
	 * @throws ElvaLexicalException 式の構文面での例外
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	public Object eval(String s, ScriptContext c) {
		return eval(new StringReader(s), c);
	}

	/**
	 * LISP処理系で使用される構文解析器の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/18
	 */
	public static final class Lexical implements Iterator<NodeBase> {
		private static final int TOKENS = 20;
		private final List<String> tokens;
		private int cursor = 0;

		/**
		 * 指定された式を走査する構文解析器を構築します。
		 *
		 *
		 * @param exp 走査対象の式
		 *
		 * @throws IOException 正規表現の読み込みに失敗した場合
		 */
		public Lexical(String exp) throws IOException {
			this.tokens = new LinkedList<>();
			final var regex = getRegexPattern();
			final var matcher = Pattern.compile(regex).matcher(exp);
			while(matcher.find()) this.tokens.add(matcher.group(1));
		}

		/**
		 * リソースファイルからLISPの字句の正規表現を取得します。
		 *
		 *
		 * @return 正規表現
		 *
		 * @throws IOException 正規表現の読み込みに失敗した場合
		 */
		public String getRegexPattern() throws IOException {
			final var stream = getClass().getResourceAsStream(PATH);
			final var reader = new InputStreamReader(stream, UTF_8);
			try(final var br = new BufferedReader(reader)) {
				return br.lines().collect(joining());
			}
		}

		/**
		 * 次の式がある場合は真を返します。
		 *
		 *
		 * @return 次の式がある場合
		 */
		@Override
		public final boolean hasNext() {
			return cursor < tokens.size();
		}

		/**
		 * 現在の位置の直前の字句をまとめた文字列を返します。
		 *
		 *
		 * @return 現在位置の直前の文字列
		 */
		public final String getLocal() {
			final int from = Math.max(cursor - TOKENS, 0);
			final var strm = tokens.subList(from, cursor);
			return strm.stream().collect(joining(" "));
		}

		/**
		 * 次の式を返します。
		 *
		 *
		 * @return 次の式
		 *
		 * @throws ElvaLexicalException 構文に問題がある場合
		 */
		@Override
		public final NodeBase next() throws ElvaLexicalException {
			final var token = tokens.get(cursor++);
			if(token.equals("(")) return nextList();
			if(token.matches("\".*\"")) return TextNode.decode(token);
			if(token.equals("'"))  return QUOTE.quote(next());
			if(token.equals("`"))  return QUASI.quote(next());
			if(token.equals(","))  return UQUOT.quote(next());
			if(token.equals(",@")) return UQSPL.quote(next());
			if(!token.equals(")")) return asNameOrReal(token);
			throw new ElvaLexicalException("isolated ')'", this);
		}

		/**
		 * 指定されたアトムを実数またはシンボルとして返します。
		 *
		 *
		 * @param atom アトム式
		 *
		 * @return 実数値または名前
		 */
		private final NodeBase asNameOrReal(String atom) {
			try {
				if(atom.contains(".")) return new RealNode(atom);
				else return new RealNode(Integer.parseInt(atom));
			} catch (NumberFormatException ex) {
				return new NameNode(atom);
			}
		}

		/**
		 * 先頭の括弧が読まれた状態で以降のリスト式を返します。
		 *
		 *
		 * @return 次のリスト式
		 *
		 * @throws ElvaLexicalException 構文に問題がある場合
		 */
		private final ListBase nextList() throws ElvaLexicalException {
			final var list = new LinkedList<NodeBase>();
			while(cursor < tokens.size()) {
				switch(tokens.get(cursor++)) {
					case ")": return new CoverSeq(list);
					default: --cursor; list.add(next());
				}
			}
			throw new ElvaLexicalException("isolated '('", this);
		}
	}
}
