/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

/**
 * LISP処理系のシステム関数の引数の個数を指定する注釈型です。
 *
 * @since 2019/05/17
 */
public @interface Arguments {
	/**
	 * 引数を評価する前の引数の最小限の個数です。
	 *
	 * @return 引数の個数
	 */
	public int min();
	/**
	 * 引数を評価する前の引数の最大限の個数です。
	 *
	 * @return 引数の個数
	 */
	public int max();
}
