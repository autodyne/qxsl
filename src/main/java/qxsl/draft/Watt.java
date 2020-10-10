/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Field;

/**
 * 交信の送信電力を表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Watt extends Qxsl<String> {
	private final String watt;

	/**
	 * 送信電力を指定して属性を構築します。
	 *
	 *
	 * @param watt 送信電力
	 */
	public Watt(String watt) {
		super(WATT);
		this.watt = watt;
	}

	@Override
	public final String value() {
		return watt;
	}
}
