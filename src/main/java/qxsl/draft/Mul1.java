/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.draft;

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
