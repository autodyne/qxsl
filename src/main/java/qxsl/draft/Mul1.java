/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Tuple;

/**
 * 交信の獲得番号を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/28
 */
public final class Mul1 extends Qxsl<String> {
	/**
	 * 獲得番号を指定して属性を構築します。
	 *
	 *
	 * @param code 獲得番号
	 */
	public Mul1(String code) {
		super(MUL1, code);
	}

	/**
	 * 交信記録の獲得番号を抽出します。
	 *
	 *
	 * @param tuple 交信記録
	 *
	 * @return 獲得番号の属性
	 *
	 * @since 2020/10/28
	 */
	public static final Mul1 from(Tuple tuple) {
		return (Mul1) tuple.get(Qxsl.MUL1);
	}
}
