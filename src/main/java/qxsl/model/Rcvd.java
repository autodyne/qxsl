/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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
