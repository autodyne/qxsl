/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.util.Objects;
import java.util.ServiceLoader;
import javax.script.ScriptException;

/**
 * ドメイン特化言語でコンテストの規約を表現する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/27
 */
public abstract class RuleKit {
	private final String name;

	/**
	 * 指定された名前の処理系を構築します。
	 *
	 * @param name 処理系の名前
	 */
	protected RuleKit(String name) {
		this.name = name;
	}

	/**
	 * この処理系の名前を返します。
	 *
	 * @return 処理系の名前
	 */
	public final String name() {
		return name;
	}

	/**
	 * この処理系が指定された名前であるか確認します。
	 *
	 * @param name 名前
	 * @return 名前が同じ場合は真
	 */
	private final boolean isName(String name) {
		return Objects.equals(name, this.name());
	}

	/**
	 * 指定された名前の処理系をライブラリから検索します。
	 *
	 * @param name 言語の名前
	 *
	 * @return 処理系 見つからない場合はnull
	 *
	 * @see ServiceLoader
	 */
	public static final RuleKit load(String name) {
		final var kits = ServiceLoader.load(RuleKit.class);
		for(var kit: kits) if(kit.isName(name)) return kit;
		return null;
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
	public abstract Contest contest(Reader reader);

	/**
	 * 指定された文字列から式を読み取って評価します。
	 * 返り値はコンテストの定義である必要があります。
	 *
	 *
	 * @param string 式
	 * @return コンテストの定義
	 *
	 * @throws UncheckedIOException 読み取りまたは評価の例外
	 */
	public abstract Contest contest(String string);
}
