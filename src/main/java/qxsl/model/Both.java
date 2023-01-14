/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.model;

import static gaas.table.QxmlFactory.BOTH;

/**
 * 交信記録で送受信局間に共通の属性を格納します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/09
 */
public final class Both extends Node {
	/**
	 * 空の要素を構築します。
	 */
	public Both() {
		super(BOTH);
	}
}
