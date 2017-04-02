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
 * 交信記録シートにおいて相手局から受信したメッセージを表現します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Rcvd extends Exch<Rcvd> {
	/**
	 * 空の{@link Rcvd}を構築します。
	 */
	public Rcvd() {
		super(QxmlFormat.RCVD);
	}

}
