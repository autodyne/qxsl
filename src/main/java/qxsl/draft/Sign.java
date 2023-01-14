/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.draft;

import java.time.ZonedDateTime;

import qxsl.model.Item;

/**
 * 交信の照合結果を表す属性の実装です。
 * 相手局の交信記録の日時を参照します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/08
 */
public final class Sign extends Qxsl<ZonedDateTime> {
	/**
	 * 時刻を指定して属性を構築します。
	 *
	 *
	 * @param time 時刻
	 */
	public Sign(ZonedDateTime time) {
		super(SIGN, time);
	}

	/**
	 * 時刻を指定して属性を構築します。
	 *
	 *
	 * @param time 時刻
	 */
	public Sign(Time time) {
		this(time.value());
	}

	/**
	 * 時刻を抽出して属性を構築します。
	 *
	 *
	 * @param item 交信記録
	 */
	public Sign(Item item) {
		this((Time) item.get(TIME));
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
