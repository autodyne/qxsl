/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
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
	 * この属性の値が有効か検証します。
	 *
	 *
	 * @return 有効な場合は真
	 *
	 * @since 2022/08/01
	 */
	@Override
	public final boolean valid() {
		final int r = (value() / 100) % 10;
		final int s = (value() / 10) % 10;
		final int t = (value() / 1) % 10;
		if(r < 1 && r > 5) return false;
		if(s < 1 && s > 9) return false;
		if(t < 1 && t > 9) return false;
		return true;
	}
}
