/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

/**
 * 無線部開発班が実装するLISP方言「Elva」の{@link ScriptEngine}を提供します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2019/05/20
 */
public final class ElvaScriptEngineFactory implements ScriptEngineFactory {
	private final String PREFIX = "elva.";
	private final String XML = "elva.xml";
	private final Properties props;

	/**
	 * LISP処理系のファクトリを構築します。
	 */
	public ElvaScriptEngineFactory() {
		this.props = new Properties();
		URL url = getClass().getResource(XML);
		try(InputStream is = url.openStream()) {
			props.loadFromXML(is);
		} catch(IOException ex) {}
	}

	@Override
	public String getEngineName() {
		return props.getProperty(ScriptEngine.ENGINE);
	}

	@Override
	public String getEngineVersion() {
		return props.getProperty(ScriptEngine.ENGINE_VERSION);
	}

	@Override
	public List<String> getExtensions() {
		final String value = props.getProperty(PREFIX + "extension");
		return Collections.unmodifiableList(Arrays.asList(value));
	}

	@Override
	public List<String> getMimeTypes() {
		final String value = props.getProperty(PREFIX + "mimetype");
		return Collections.unmodifiableList(Arrays.asList(value));
	}

	@Override
	public List<String> getNames() {
		final String value = props.getProperty(ScriptEngine.NAME);
		return Collections.unmodifiableList(Arrays.asList(value));
	}

	@Override
	public String getLanguageName() {
		return props.getProperty(ScriptEngine.LANGUAGE);
	}

	@Override
	public String getLanguageVersion() {
		return props.getProperty(ScriptEngine.LANGUAGE_VERSION);
	}

	@Override
	public String getParameter(String key) {
		return props.getProperty(key);
	}

	/**
	 * 現在のElvaではJavaのメソッドを呼び出す方法が未実装です。
	 *
	 * @param obj 対象となるメソッドを有するオブジェクトの名前
	 * @param m 呼び出す対象のメソッドの名前
	 * @param args 引数
	 * 
	 * @return null
	 */
	@Override
	public String getMethodCallSyntax(String obj, String m, String...keys) {
		return null;
	}

	/**
	 * 現在のElvaでは標準出力に文字列を書き出す機能が未実装です。
	 *
	 * @param toDisplay 出力する文字列
	 * @return null
	 */
	@Override
	public String getOutputStatement(String toDisplay) {
		return null;
	}

	@Override
	public String getProgram(String...statements) {
		return String.format("(progn %s)", String.join(" ", statements));
	}

	@Override
	public ScriptEngine getScriptEngine() {
		return new ElvaScriptEngine();
	}
}
