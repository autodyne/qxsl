/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import gaas.utils.AssetUtils;

/**
 * ドメイン特化言語でコンテストを定義します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
public final class RuleKit {
	private final ScriptEngine engine;

	/**
	 * 指定された言語処理系を利用します。
	 *
	 *
	 * @param engine 言語処理系
	 */
	private RuleKit(ScriptEngine engine) {
		this.engine = engine;
	}

	/**
	 * 処理系が使う言語の名前を返します。
	 *
	 *
	 * @return 言語の名前
	 */
	public final String name() {
		return engine.getFactory().getLanguageName();
	}

	/**
	 * 指定された言語名の処理系を検索して返します。
	 *
	 *
	 * @param name 言語名
	 *
	 * @return 処理系
	 *
	 * @see ScriptEngineManager
	 */
	public static final RuleKit forName(String name) {
		final var man = new ScriptEngineManager();
		final var kit = man.getEngineByName(name);
		return kit != null? new RuleKit(kit): null;
	}

	/**
	 * 指定された拡張子の処理系を検索して返します。
	 *
	 *
	 * @param path 拡張子またはファイルの名前
	 *
	 * @return 処理系
	 *
	 * @see ScriptEngineManager
	 */
	public static final RuleKit forFile(String path) {
		final var man = new ScriptEngineManager();
		final int idx = path.lastIndexOf('.') + 1;
		final var ext = path.substring(idx);
		final var kit = man.getEngineByExtension(ext);
		return kit != null? new RuleKit(kit): null;
	}

	/**
	 * 指定された文字列からライブラリの定義を読み取ります。
	 *
	 *
	 * @param string 式を提供する文字列
	 *
	 * @return ライブラリの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public final Library eval(String string) {
		return eval(new StringReader(string));
	}

	/**
	 * 指定されたリーダからライブラリの定義を読み取ります。
	 *
	 *
	 * @param reader 式を提供するリーダ
	 *
	 * @return ライブラリの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public final Library eval(Reader reader) {
		try {
			return (Library) engine.eval(reader);
		} catch (ScriptException ex) {
			final var io = new IOException(ex);
			throw new UncheckedIOException(io);
		}
	}

	/**
	 * 指定された名前の内蔵ライブラリの定義を読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return ライブラリの定義 未定義の場合はnull
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public static final Library load(String path) {
		final var lib = new AssetUtils(Library.class);
		return forFile(path).eval(lib.string(path));
	}
}
