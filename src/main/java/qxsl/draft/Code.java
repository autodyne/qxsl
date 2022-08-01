/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Tuple;

/**
 * 交信のナンバーを表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/09
 */
public final class Code extends Qxsl<String> {
	/**
	 * ナンバーを指定して属性を構築します。
	 *
	 *
	 * @param code ナンバー
	 */
	public Code(String code) {
		super(CODE, code);
	}

	/**
	 * 交信記録のナンバーを抽出します。
	 *
	 *
	 * @param tuple 交信記録
	 *
	 * @return ナンバーの属性
	 *
	 * @since 2020/10/28
	 */
	public static final Code from(Tuple tuple) {
		return (Code) tuple.get(Qxsl.CODE);
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
