/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

/**
 * 得点計算の処理に成功した場合に返されます。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2016/11/26
 */
public final class Success implements Message {
	private static final long serialVersionUID = 1L;

	/**
	 * 交信回数の数え上げに使用される鍵です。
	 */
	public final Object call;

	/**
	 * マルチ数の数え上げに使用される鍵です。
	 */
	public final Object mult;

	/**
	 * この交信により得られる得点です。
	 */
	public final int score;

	/**
	 * 交信回数とマルチ数の鍵から得点を構築します。
	 *
	 * @param call 交信回数の鍵
	 * @param mult マルチ数の鍵
	 * @param score 得点
	 */
	public Success(Object call, Object mult, int score) {
		this.call = call;
		this.mult = mult;
		this.score = score;
	}
}
