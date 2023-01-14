/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.utils;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 文字列とモールス符号の変換器です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2021/09/15
 */
public final class MorseCode {
	private final Map<Character, String> map;

	/**
	 * 変換器を構築します。
	 */
	public MorseCode() {
		this.map = new HashMap<>();
		final var util = AssetUtil.from(this);
		for(var v: util.listLines("morse.dat")) {
			map.put(v.charAt(0), v.substring(1));
		}
	}

	/**
	 * 指定された文字をモールス符号に変換します。
	 *
	 *
	 * @param ch 文字
	 *
	 * @return 符号語 (長点は_ 短点は. 語尾は;)
	 */
	public final String encode(int ch) {
		return encode((char) ch);
	}

	/**
	 * 指定された文字をモールス符号に変換します。
	 *
	 *
	 * @param ch 文字
	 *
	 * @return 符号語 (長点は_ 短点は. 語尾は;)
	 */
	public final String encode(char ch) {
		return map.get(Character.toUpperCase(ch));
	}

	/**
	 * 指定された文字列をモールス符号に変換します。
	 *
	 *
	 * @param text 文字列
	 *
	 * @return 空白文字で区切られた符号語の列
	 */
	public final String encode(String text) {
		final var join = Collectors.joining(" ");
		return text.chars().mapToObj(this::encode).collect(join);
	}
}
