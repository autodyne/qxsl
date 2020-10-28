/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Tuple;

/**
 * 交信のレポートを表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class RSTQ extends Qxsl<Integer> {
	/**
	 * 指定された整数値で属性を構築します。
	 *
	 *
	 * @param rst 整数値
	 */
	public RSTQ(int rst) {
		super(RSTQ, rst);
	}

	/**
	 * 指定された整数値で属性を構築します。
	 *
	 *
	 * @param r 了解度
	 * @param s 信号強度
	 * @param t 音調品質
	 */
	public RSTQ(int r, int s, int t) {
		this(r * 100 + s * 10 + t);
	}

	/**
	 * 交信記録のレポートを抽出します。
	 *
	 *
	 * @param tuple 交信記録
	 *
	 * @return レポートの属性
	 *
	 * @since 2020/10/28
	 */
	public static final RSTQ from(Tuple tuple) {
		return (RSTQ) tuple.get(Qxsl.RSTQ);
	}

	/**
	 * 指定された整数値がレポートであるかを確認します。
	 *
	 *
	 * @param rst 値
	 *
	 * @return 正規形のレポートに変換可能な場合は真
	 *
	 * @since 2020/10/28
	 */
	public static final boolean isValid(int rst) {
		final int r = (rst / 100) % 10;
		final int s = (rst / 10) % 10;
		final int t = (rst / 1) % 10;
		if(r < 1 && r > 5) return false;
		if(s < 1 && s > 9) return false;
		if(t < 1 && t > 9) return false;
		return true;
	}
}
