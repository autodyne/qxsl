/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.ruler;

import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import javax.script.ScriptException;

import elva.lang.ElvaLisp;
import elva.warn.ElvaLexicalException;
import elva.warn.ElvaRuntimeException;

import qxsl.ruler.Contest;

/**
 * ドメイン特化のLISPでコンテストの規約を表現する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 *
 * @see ElvaLisp 内部で使用されるLISP処理系
 */
public final class ElvaRuleKit extends qxsl.ruler.RuleKit {
	private final ElvaLisp engine;

	/**
	 * LISP処理系を構築します。
	 */
	public ElvaRuleKit() {
		super("elva");
		this.engine = new ElvaLisp();
	}

	/**
	 * 指定されたリーダから式を読み取って評価します。
	 * 返り値はコンテストの定義である必要があります。
	 *
	 *
	 * @param reader 式を読み取るリーダ
	 *
	 * @return コンテストの定義
	 *
	 * @throws UncheckedIOException 式の読み取りの例外
	 * @throws ElvaLexicalException 式の構文面での例外
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	@Override
	public final Contest contest(final Reader reader) {
		try {
			return (Contest) engine.eval(reader);
		} catch (ScriptException ex) {
			throw new ElvaRuntimeException(ex);
		}
	}

	/**
	 * 指定された文字列から式を読み取って評価します。
	 * 返り値はコンテストの定義である必要があります。
	 *
	 *
	 * @param string 式
	 *
	 * @return コンテストの定義
	 *
	 * @throws UncheckedIOException 式の読み取りの例外
	 * @throws ElvaLexicalException 式の構文面での例外
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	@Override
	public final Contest contest(final String string) {
		return contest(new StringReader(string));
	}
}
