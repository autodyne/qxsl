/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.model;

import static gaas.table.QxmlFactory.RCVD;

/**
 * 交信記録で相手局から受信した属性を格納します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Rcvd extends Node {
	/**
	 * 空の要素を構築します。
	 */
	public Rcvd() {
		super(RCVD);
	}
}
