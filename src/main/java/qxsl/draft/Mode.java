/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Tuple;

/**
 * 交信の通信方式を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Mode extends Qxsl<String> {
	/**
	 * 通信方式を指定して属性を構築します。
	 *
	 *
	 * @param mode 通信方式
	 */
	public Mode(String mode) {
		super(MODE, mode.toUpperCase());
	}

	/**
	 * 通信方式が聴覚電信か確認します。
	 *
	 *
	 * @return CWの場合は真
	 *
	 * @since 2020/10/28
	 */
	public final boolean isCW() {
		return value.equals("CW");
	}

	/**
	 * 通信方式が印字電信か確認します。
	 *
	 *
	 * @return RTTYの場合は真
	 *
	 * @since 2020/10/28
	 */
	public final boolean isRTTY() {
		return value.equals("RTTY");
	}

	/**
	 * 交信記録の通信方式を抽出します。
	 *
	 *
	 * @param tuple 交信記録
	 *
	 * @return 通信方式の属性
	 *
	 * @since 2020/10/28
	 */
	public static final Mode from(Tuple tuple) {
		return (Mode) tuple.get(Qxsl.MODE);
	}
}
