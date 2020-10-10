/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

/**
 * コンテストで相手局と交換するシリアル番号です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/09
 */
public final class Code extends Qxsl<String> {
	private final String value;

	/**
	 * シリアル番号を指定して属性を構築します。
	 *
	 *
	 * @param code シリアル番号
	 */
	public Code(String code) {
		super(CODE);
		this.value = code;
	}

	@Override
	public final String value() {
		return value;
	}
}
