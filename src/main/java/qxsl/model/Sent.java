/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import qxsl.table.secret.QxmlFormat;

/**
 * 交信記録シートにおいて相手局に送信したメッセージを表現します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Sent extends Exch<Sent> {
	/**
	 * 空の{@link Sent}を構築します。
	 */
	public Sent() {
		super(QxmlFormat.SENT);
	}

}
