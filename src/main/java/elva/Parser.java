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
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import elva.ElvaLisp.ElvaLexicalException;

/**
 * LISP処理系の構文解析器の実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/18
 */
final class Parser implements Iterator<Object> {
	private final List<String> allTokens;
	private int cursor = 0;

	/**
	 * 指定された式を走査する構文解析器を構築します。
	 *
	 * @param exp 走査対象の式
	 * 
	 * @throws IOException 正規表現の読み込みに失敗した場合
	 */
	public Parser(String exp) throws IOException {
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
	public final Object next() throws ElvaLexicalException {
		final String atom = allTokens.get(cursor++);
		if(atom.equals("(")) return nextList();
		if(atom.matches("\".*\"")) return escape(atom);
		if(atom.equals("'"))  return new Struct(Quotes.QUOTE, next());
		if(atom.equals("`"))  return new Struct(Quotes.QUASI, next());
		if(atom.equals(","))  return new Struct(Quotes.UQUOT, next());
		if(atom.equals(",@")) return new Struct(Quotes.UQSPL, next());
		if(!atom.equals(")")) return asSymbolOrReal(atom);
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
	 * @return 実数値 もしくはシンボル
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
	private final Struct nextList() throws ElvaLexicalException {
		final ArrayList<Object> list = new ArrayList<>();
		boolean closed = false;
		loop: while(cursor < allTokens.size()) {
			switch(allTokens.get(cursor++)) {
				case ")": closed = true; break loop;
				default: --cursor; list.add(next());
			}
		}
		if(closed) return Struct.of(list);
		throw new ElvaLexicalException("isolated '('", this);
	}
}
