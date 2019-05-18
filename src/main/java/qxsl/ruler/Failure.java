/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import java.io.Serializable;
import qxsl.model.Item;

/**
 * 得点計算の処理に失敗した場合に返されます。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2016/11/26
 */
public final class Failure implements Message, Serializable {
	private static final long serialVersionUID = 1L;
	private final String text;
	private final Item item;
	
	/**
	 * 指定した内容の{@link Failure}を構築します。
	 *
	 * @param text 失敗の内容を説明する文字列
	 * @param item 関連づけられる交信
	 */
	public Failure(String text, Item item) {
		this.text = text;
		this.item = item;
	}

	/**
	 * 失敗の内容を説明する文字列を返します。
	 * 
	 * @return メッセージ
	 */
	public final String getMessage() {
		return text;
	}

	@Override
	public final Item item() {
		return item;
	}
}
