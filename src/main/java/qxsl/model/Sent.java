/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import static qxsl.extra.table.QxmlFactory.SENT;

/**
 * 交信記録で相手局まで送信した情報を格納します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Sent extends Exch {
	/**
	 * 指定した親を持つ要素を構築します。
	 *
	 *
	 * @param item 親
	 */
	Sent(Item item) {
		super(item, SENT);
	}
}
