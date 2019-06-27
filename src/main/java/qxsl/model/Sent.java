/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import static qxsl.extra.table.QxmlFormat.SENT;

/**
 * 交信記録で相手局に送信した情報を格納する{@link Tuple}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Sent extends Exch {
	/**
	 * 空の{@link Sent}を構築します。
	 */
	public Sent() {
		super(SENT);
	}

}
