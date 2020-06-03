/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Serializable;
import qxsl.model.Item;

/**
 * ライブラリに内蔵されるLISPの処理結果を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/12/05
 */
public interface Message extends Serializable {
	/**
	 * 交信の実体を表す{@link Item}を返します。
	 *
	 * @return 交信の実体
	 */
	public Item item();
}
