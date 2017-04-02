/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;

/**
 * コンテストの部門を表現します。交信の有効と無効を判別します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2016/11/25
 */
public abstract class Section {
	/**
	 * 部門名を返します。
	 *
	 * @return 部門名
	 */
	public abstract String getName();

	/**
	 * UIで表示するために部門名を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * 指定された{@link Item}が通過するか確認します。
	 *
	 * @param item 承認を要する交信記録
	 * @return 通過する場合true
	 * @throws Exception LISPの評価で発生した何らかの例外
	 */
	public abstract Message validate(Item item) throws Exception;
}
