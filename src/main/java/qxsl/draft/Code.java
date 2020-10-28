/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Tuple;

/**
 * 交信の交換番号を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/09
 */
public final class Code extends Qxsl<String> {
	/**
	 * 交換番号を指定して属性を構築します。
	 *
	 *
	 * @param code 交換番号
	 */
	public Code(String code) {
		super(CODE, code);
	}

	/**
	 * 交信記録の交換番号を抽出します。
	 *
	 *
	 * @param tuple 交信記録
	 *
	 * @return 交換番号の属性
	 *
	 * @since 2020/10/28
	 */
	public static final Code from(Tuple tuple) {
		return (Code) tuple.get(Qxsl.CODE);
	}
}
