/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.draft;

/**
 * 交信の送信電力を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Watt extends Qxsl<String> {
	/**
	 * 送信電力を指定して属性を構築します。
	 *
	 *
	 * @param watt 送信電力
	 */
	public Watt(String watt) {
		super(WATT, watt);
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
		switch(value()) {
			case "P": return true;
			case "L": return true;
			case "M": return true;
			case "H": return true;
			default: return false;
		}
	}
}
