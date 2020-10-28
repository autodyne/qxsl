/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Tuple;

/**
 * 運用者の個人名を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Name extends Qxsl<String> {
	/**
	 * 運用者名を指定して属性を構築します。
	 *
	 *
	 * @param name 運用者名
	 */
	public Name(String name) {
		super(NAME, name);
	}

	/**
	 * 交信記録の運用者名を抽出します。
	 *
	 *
	 * @param tuple 交信記録
	 *
	 * @return 運用者名の属性
	 *
	 * @since 2020/10/28
	 */
	public static final Name from(Tuple tuple) {
		return (Name) tuple.get(Qxsl.NAME);
	}
}
