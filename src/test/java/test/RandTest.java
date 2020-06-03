/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package test;

import java.security.SecureRandom;

/**
 * 任意の文字列を擬似乱数で生成するクラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/06/14
 *
 */
public class RandTest {
	private final SecureRandom rand = new SecureRandom();

	/**
	 * 指定された整数値未満の正の整数を返します。
	 *
	 * @param max 最大値 (含まない)
	 * @return 整数
	 */
	public final int randInt(int max) {
		return rand.nextInt(max);
	}

	/**
	 * 任意の英数字で構成される文字列を所定の最大文字数で返します。
	 *
	 * @param max 最大文字数
	 * @return 文字列
	 */
	public final String alnum(int max) {
		char[] text = new char[1+rand.nextInt(max)];
		for(int i=0; i<text.length; i++) switch(rand.nextInt(3)) {
			case 0: text[i] = (char)('0' + rand.nextInt(10)); break;
			case 1: text[i] = (char)('A' + rand.nextInt(26)); break;
			case 2: text[i] = (char)('a' + rand.nextInt(26)); break;
		}
		return new String(text);
	}
}
