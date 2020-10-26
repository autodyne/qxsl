/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

/**
 * ドメイン特化言語でコンテストの規約を表現する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
public final class RuleKit {
	private final ScriptEngine engine;

	/**
	 * 指定された名前と拡張子の処理系を構築します。
	 *
	 *
	 * @param name 言語名
	 * @param exts 拡張子
	 */
	private RuleKit(ScriptEngine engine) {
		this.engine = engine;
	}

	/**
	 * この処理系の言語名を返します。
	 *
	 *
	 * @return 言語名
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
	 * 指定された文字列からコンテストの規約を読み取ります。
	 *
	 *
	 * @param string 式を提供する文字列
	 *
	 * @return コンテストの規約
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public final Contest evalAsContest(String string) {
		return evalAsContest(new StringReader(string));
	}

	/**
	 * 指定された文字列からコンテストの部門を読み取ります。
	 *
	 *
	 * @param string 式を提供する文字列
	 *
	 * @return コンテストの部門
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public final Section evalAsSection(String string) {
		return evalAsSection(new StringReader(string));
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
	public final Library evalAsLibrary(String string) {
		return evalAsLibrary(new StringReader(string));
	}

	/**
	 * 指定されたリーダからコンテストの規約を読み取ります。
	 *
	 *
	 * @param reader 式を提供するリーダ
	 *
	 * @return コンテストの規約
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public final Contest evalAsContest(Reader reader) {
		return (Contest) evalAsLibrary(reader);
	}

	/**
	 * 指定されたリーダからコンテストの部門を読み取ります。
	 *
	 *
	 * @param reader 式を提供するリーダ
	 *
	 * @return コンテストの部門
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public final Section evalAsSection(Reader reader) {
		return (Section) evalAsLibrary(reader);
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
	public final Library evalAsLibrary(Reader reader) {
		try {
			return (Library) engine.eval(reader);
		} catch (ScriptException ex) {
			final var io = new IOException(ex);
			throw new UncheckedIOException(io);
		}
	}

	/**
	 * 指定された名前の内蔵コンテストの規約を読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return コンテストの規約
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public static final Contest loadAsContest(String path) {
		return (Contest) loadAsLibrary(path);
	}

	/**
	 * 指定された名前の内蔵コンテストの部門を読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return コンテストの部門
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public static final Section loadAsSection(String path) {
		return (Section) loadAsLibrary(path);
	}

	/**
	 * 指定された名前の内蔵ライブラリの定義を読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return ライブラリの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public static final Library loadAsLibrary(String path) {
		final var kit = Optional.ofNullable(forFile(path));
		return kit.map(k -> k.resource(path)).orElse(null);
	}

	/**
	 * 交信記録の構造を変換する内蔵ライブラリを準備します。
	 *
	 *
	 * @return ライブラリ
	 */
	public static final Library getFormatUtils() {
		return loadAsLibrary("format.lisp");
	}

	/**
	 * 指定された名前の内蔵ライブラリの定義を読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return ライブラリの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public final Library resource(String path) {
		try(var res = getResourceAsReader(path)) {
			return evalAsLibrary(res);
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * 指定された名前のリソースを読み取るリーダを返します。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return リーダ
	 */
	private static final Reader getResourceAsReader(String path) {
		final var strm = RuleKit.class.getResourceAsStream(path);
		return new InputStreamReader(strm, StandardCharsets.UTF_8);
	}
}
