/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * LISP処理系のシステム関数の引数の個数を指定する注釈型です。
 *
 *
 * @author Journal of Hamradio Informatics
 *
 * @since 2019/05/17
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Params {
	/**
	 * 評価前の引数の最小限の個数です。
	 *
	 * @return 引数の個数
	 */
	public int min();
	/**
	 * 評価前の引数の最大限の個数です。
	 *
	 * @return 引数の個数
	 */
	public int max();
}
