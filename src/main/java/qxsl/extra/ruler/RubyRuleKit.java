/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.ruler;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import qxsl.ruler.Contest;

/**
 * ドメイン特化のRubyでコンテストの規約を表現する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/07/19
 */
public final class RubyRuleKit extends qxsl.ruler.RuleKit {
	private final Class<?> CLS = Contest.class;
	private final ScriptEngine engine;

	/**
	 * Ruby処理系を構築します。
	 */
	public RubyRuleKit() {
		super("ruby");
		final var man = new ScriptEngineManager();
		this.engine = man.getEngineByName("ruby");
	}

	/**
	 * 指定されたリーダから式を読み取って評価します。
	 * 返り値はコンテストの定義である必要があります。
	 *
	 *
	 * @param reader 式を読み取るリーダ
	 * @return コンテストの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	@Override
	public final Contest contest(final Reader reader) {
		final var init = CLS.getResourceAsStream("common.rb");
		try (init; reader) {
			engine.eval(new InputStreamReader(init));
			return (Contest) engine.eval(reader);
		} catch (ScriptException | IOException ex) {
			throw new UncheckedIOException(new IOException(ex));
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
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	@Override
	public final Contest contest(final String string) {
		return contest(new StringReader(string));
	}
}
