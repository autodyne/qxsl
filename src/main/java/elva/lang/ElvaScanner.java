/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.script.*;

import elva.core.*;
import elva.warn.*;

/**
 * LISP処理系で使用される構文解析器の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class ElvaScanner implements Iterator<ElvaNode> {
	private final List<String> allTokens;
	private int cursor = 0;

	/**
	 * 指定された式を走査する構文解析器を構築します。
	 *
	 * @param exp 走査対象の式
	 *
	 * @throws IOException 正規表現の読み込みに失敗した場合
	 */
	public ElvaScanner(String exp) throws IOException {
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
		final var r = getClass().getResourceAsStream("elva.lex");
		try(var b = new BufferedReader(new InputStreamReader(r))) {
			return b.lines().collect(Collectors.joining());
		}
	}

	/**
	 * 次の式がある場合は真を返します。
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
	public final ElvaNode next() throws ElvaLexicalException {
		final String atom = allTokens.get(cursor++);
		if(atom.equals("(")) return nextList();
		if(atom.matches("\".*\"")) return ElvaText.decode(atom);
		if(atom.equals("'"))  return ElvaName.Quote.QUOTE.quote(next());
		if(atom.equals("`"))  return ElvaName.Quote.QUASI.quote(next());
		if(atom.equals(","))  return ElvaName.Quote.UQUOT.quote(next());
		if(atom.equals(",@")) return ElvaName.Quote.UQSPL.quote(next());
		if(atom.equals("!"))  return ElvaName.Quote.CONST.quote(next());
		if(!atom.equals(")")) return ElvaNode.wrap(asNameOrReal(atom));
		throw new ElvaLexicalException("isolated ')'", this);
	}

	/**
	 * 指定されたアトムを実数またはシンボルとして返します。
	 *
	 * @param atom アトム式
	 * @return 実数値または名前
	 */
	private final ElvaNode asNameOrReal(String atom) {
		try {
			if(atom.contains(".")) return new ElvaReal(atom);
			else return new ElvaReal(Integer.parseInt(atom));
		} catch (NumberFormatException ex) {
			return new ElvaName(atom);
		}
	}

	/**
	 * 先頭の括弧が読まれた状態で以降のリスト式を返します。
	 *
	 * @return 次のリスト式
	 * @throws ElvaLexicalException 構文に問題がある場合
	 */
	private final ElvaList nextList() throws ElvaLexicalException {
		final var list = new ArrayList<ElvaNode>();
		boolean closed = false;
		loop:
		while(cursor < allTokens.size()) {
			switch(allTokens.get(cursor++)) {
				case ")": closed = true; break loop;
				default: --cursor; list.add(next());
			}
		}
		if(closed) return ElvaList.chain(list);
		throw new ElvaLexicalException("isolated '('", this);
	}
}
