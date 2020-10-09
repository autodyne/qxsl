/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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
