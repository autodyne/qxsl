/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package util;

import java.security.SecureRandom;

/**
 * 任意の文字列を擬似乱数で生成するクラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/06/14
 *
 */
public final class RandText {
	private static final SecureRandom rand = new SecureRandom();
	/**
	 * 任意の英数字で構成される文字列を所定の最大文字数で返します。
	 *
	 * @param max 最大文字数
	 * @return 文字列
	 */
	public static final String alnum(int max) {
		char[] text = new char[1+rand.nextInt(max)];
		for(int i=0; i<text.length; i++) switch(rand.nextInt(3)) {
			case 0: text[i] = (char)('0' + rand.nextInt(10)); break;
			case 1: text[i] = (char)('A' + rand.nextInt(26)); break;
			case 2: text[i] = (char)('a' + rand.nextInt(26)); break;
		}
		return new String(text);
	}
}
