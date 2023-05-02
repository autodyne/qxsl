/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.utils;

import java.util.HashMap;
import java.util.Map;
import java.util.StringJoiner;

/**
 * 欧字列をモールス符号の語列に変換します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2021/09/15
 */
public final class MorseCode {
	private final Map<String, String> en;
	private final Map<String, String> de;

	/**
	 * 変換器を構築します。
	 */
	public MorseCode() {
		this.en = new HashMap<>();
		this.de = new HashMap<>();
		final var util = AssetUtil.from(this);
		for(var v: util.listLines("morse.dat")) {
			en.put(v.substring(0, 1), v.substring(1));
			de.put(v.substring(1), v.substring(0, 1));
		}
	}

	/**
	 * 指定された欧字列を符号語の列に変換します。
	 *
	 *
	 * @param text 欧字列
	 *
	 * @return 符号語の列
	 */
	public final String encode(String text) {
		final var join = new StringJoiner(" ");
		final var list = text.split("");
		for(var v: list) join.add(en.get(v));
		return join.toString().toUpperCase();
	}

	/**
	 * 指定された符号語の列を欧字列に変換します。
	 *
	 *
	 * @param code 符号語の列
	 *
	 * @return 欧字列
	 */
	public final String decode(String code) {
		final var join = new StringJoiner("");
		final var list = code.split(" +");
		for(var v: list) join.add(de.get(v));
		return join.toString().toUpperCase();
	}
}
