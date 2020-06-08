/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Serializable;
import java.util.StringJoiner;
import qxsl.model.Item;

/**
 * 規約により拒否された交信に付与されます。
 * 総得点や乗数の計算には反映されません。
 *
 *
 * @author 無線部開発班
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
	 * @param item 関連づけられる交信
	 * @param text 失敗の内容を説明する文字列
	 */
	public Failure(Item item, Object text) {
		this.item = item;
		this.text = text.toString();
	}

	/**
	 * 失敗の内容を説明する文字列を返します。
	 *
	 * @return 文字列
	 */
	public final String text() {
		return text;
	}

	@Override
	public final Item item() {
		return item;
	}

	/**
	 * この交信の文字列による表現を返します。
	 *
	 * @return 文字列
	 */
	public final String toString() {
		StringJoiner sj = new StringJoiner(" ");
		sj.add(Failure.class.getCanonicalName());
		sj.add(String.format("message:%s", text()));
		return String.format("{%s}", sj.toString());
	}
}
