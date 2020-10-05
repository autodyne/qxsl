/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.ruler;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import qxsl.ruler.Library;
import qxsl.ruler.RuleKit;

/**
 * ドメイン特化のRubyでコンテストの規約を表現する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/07/19
 */
public final class RubyRuleKit extends RuleKit {
	private final ScriptEngine engine;

	/**
	 * 処理系を構築します。
	 *
	 *
	 * @since 2020/07/19
	 */
	public RubyRuleKit() {
		super("ruby");
		final var man = new ScriptEngineManager();
		this.engine = man.getEngineByName("ruby");
	}

	/**
	 * 指定されたリーダから式を読み取って評価します。
	 * 返り値はライブラリの定義である必要があります。
	 *
	 *
	 * @param reader 式を読み取るリーダ
	 * @return ライブラリの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	@Override
	public final Library library(final Reader reader) {
		try {
			return (Library) engine.eval(reader);
		} catch (ScriptException ex) {
			final var io = new IOException(ex);
			throw new UncheckedIOException(io);
		}
	}

	/**
	 * 指定された文字列から式を読み取って評価します。
	 * 返り値はライブラリの定義である必要があります。
	 *
	 *
	 * @param string 式
	 *
	 * @return ライブラリの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	@Override
	public final Library library(final String string) {
		return library(new StringReader(string));
	}
}
