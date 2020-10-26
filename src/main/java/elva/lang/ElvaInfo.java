/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.List;
import java.util.Properties;
import java.util.StringJoiner;
import javax.script.ScriptEngineFactory;

import static javax.script.ScriptEngine.ENGINE;
import static javax.script.ScriptEngine.ENGINE_VERSION;
import static javax.script.ScriptEngine.LANGUAGE;
import static javax.script.ScriptEngine.LANGUAGE_VERSION;

/**
 * 無線部開発班が実装するLISP処理系をインスタンス化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/07
 */
public final class ElvaInfo implements ScriptEngineFactory {
	private final Properties conf;

	/**
	 * 各種設定を読み出してファクトリを構築します。
	 *
	 *
	 * @throws UncheckedIOException 設定の例外
	 */
	public ElvaInfo() throws UncheckedIOException {
		this.conf = new Properties();
		install("elva.xml");
	}

	/**
	 * 指定された名前の設定ファイルを読み出します。
	 *
	 *
	 * @param name 設定ファイルの名前
	 *
	 * @throws UncheckedIOException 設定の例外
	 */
	private final void install(String name) {
		final var type = getClass();
		final var path = type.getResource(name);
		try(final var is = path.openStream()) {
			conf.loadFromXML(is);
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * この処理系の名前を返します。
	 *
	 *
	 * @return 処理系の名前
	 */
	@Override
	public String getEngineName() {
		return getParameter(ENGINE);
	}

	/**
	 * この処理系の版数を返します。
	 *
	 *
	 * @return 処理系の版数
	 */
	@Override
	public String getEngineVersion() {
		return getParameter(ENGINE_VERSION);
	}

	/**
	 * この処理系が対応する言語の名前を返します。
	 *
	 *
	 * @return 言語の名前
	 */
	@Override
	public String getLanguageName() {
		return getParameter(LANGUAGE);
	}

	/**
	 * この処理系が対応する言語の版数を返します。
	 *
	 *
	 * @return 言語の版数
	 */
	@Override
	public String getLanguageVersion() {
		return getParameter(LANGUAGE_VERSION);
	}

	/**
	 * この処理系の可能な名前のリストを返します。
	 *
	 *
	 * @return 名前のリスト
	 */
	@Override
	public List<String> getNames() {
		return List.of(getEngineName());
	}

	/**
	 * この言語が対応する形式のリストを返します。
	 *
	 *
	 * @return 媒体のリスト
	 */
	@Override
	public List<String> getMimeTypes() {
		return List.of();
	}

	/**
	 * この言語が定める拡張子のリストを返します。
	 *
	 *
	 * @return 拡張子のリスト
	 */
	@Override
	public List<String> getExtensions() {
		return List.of("lisp");
	}

	/**
	 * 指定された属性に紐付けられた値を返します。
	 *
	 *
	 * @param name 属性の名前
	 *
	 * @return 設定値
	 */
	@Override
	public String getParameter(String name) {
		return conf.getProperty(name);
	}

	/**
	 * 処理系のインスタンスを生成します。
	 *
	 *
	 * @return 処理系
	 */
	@Override
	public ElvaLisp getScriptEngine() {
		return new ElvaLisp();
	}

	/**
	 * 式を順番に評価する式を生成します。
	 *
	 *
	 * @param list 式のリスト
	 *
	 * @return 式
	 */
	public String getProgram(String...list) {
		final var join = new StringJoiner(" ");
		for(String state: list) join.add(state);
		return String.format("(block %s)", join);
	}

	/**
	 * 標準出力に書き込む式を生成します。
	 *
	 *
	 * @param text 書き込む文字列
	 *
	 * @return 式
	 */
	@Override
	public String getOutputStatement(String text) {
		return String.format("(print %s)", text);
	}

	/**
	 * 指定されたメソッドを実行する式を返します。
	 *
	 *
	 * @param obj 対象となるオブジェクト
	 * @param met メソッドの名前
	 * @param seq 引数
	 *
	 * @return 式
	 */
	@Override
	public String getMethodCallSyntax(String obj, String met, String...seq) {
		final var sexp = new StringJoiner(" ", "(", ")");
		sexp.add("invoke").add("'".concat(met)).add(obj);
		return sexp.add(String.join(" ", seq)).toString();
	}
}
