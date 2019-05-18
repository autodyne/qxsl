/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import javax.script.ScriptException;

/**
 * コンテストの規約を表現し、{@link Section}を提供します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2016/11/25
 */
public abstract class Contest implements Iterable<Section> {
	/**
	 * コンテスト名を返します。
	 *
	 * @return コンテスト名
	 */
	public abstract String getName();

	/**
	 * UIで表示するためにコンテスト名を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * このコンテストが提供する部門をイテレータで返します。
	 * 
	 * @return 全ての部門を含むイテレータ
	 */
	public abstract Iterator<Section> iterator();

	/**
	 * 指定された名前の{@link Section}を返します。
	 *
	 * @param name 部門の名前
	 * @return 該当する部門 見つからない場合はnull
	 */
	public final Section getSection(String name) {
		for(Section sec: this) {
			if(sec.getName().equals(name)) return sec;
		}
		return null;
	}

	/**
	 * 指定された{@link Contest}をライブラリから読み出します。
	 *
	 * @param name コンテストを定義したファイルの名前
	 * @return ライブラリに内蔵されたコンテストの定義
	 * 
	 * @throws ScriptException コンテスト定義読み取り時の例外
	 */
	public static final Contest defined(String name) throws ScriptException {
		final URL url = Contest.class.getResource(name);
		InputStreamReader reader = null;
		try(InputStream is = url.openStream()) {
			reader = new InputStreamReader(is, "UTF-8");
			return (Contest) new RuleKit().eval(reader);
		} catch(IOException ex) {
			throw new ScriptException(ex);
		}
	}
}
