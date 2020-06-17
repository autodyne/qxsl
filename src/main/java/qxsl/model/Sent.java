/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import static qxsl.extra.table.QxmlFormat.SENT;

/**
 * 交信記録で相手局まで送信した情報を格納する{@link Tuple}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Sent extends Exch {
	/**
	 * 指定した親の隷下に{@link Rcvd}を構築します。
	 *
	 * @param item 親
	 */
	protected Sent(Item item) {
		super(item, SENT);
	}

}
