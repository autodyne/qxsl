/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Tuple;

/**
 * 更なる獲得番号を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/28
 */
public final class Mul2 extends Qxsl<String> {
	/**
	 * 獲得番号を指定して属性を構築します。
	 *
	 *
	 * @param code 獲得番号
	 */
	public Mul2(String code) {
		super(MUL2, code);
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
	public static final Mul2 from(Tuple tuple) {
		return (Mul2) tuple.get(Qxsl.MUL2);
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
		return true;
	}
}
