/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.draft;

/**
 * 交信記録の補足を表す属性の実装です。
 * この属性は備考よりも優先されません。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/06
 */
public final class Text extends Qxsl<String> {
	/**
	 * 補足を指定して属性を構築します。
	 *
	 *
	 * @param note 補足
	 */
	public Text(String note) {
		super(TEXT, note);
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
