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
 * 総得点やマルチの計算には反映されません。
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
	 * 交信の実体と失敗の内容を説明する文字列を設定します。
	 *
	 *
	 * @param item 関連づけられる交信
	 * @param text 失敗の内容を説明する文字列
	 */
	public Failure(Item item, Object text) {
		this.item = item;
		this.text = text.toString();
	}

	/**
	 * 交信の成立により得られる素点を返します。
	 *
	 *
	 * @return 交信1件の得点
	 */
	@Override
	public final int score() {
		return 0;
	}

	/**
	 * 交信の実体を表す交信記録を返します。
	 *
	 *
	 * @return 交信の実体
	 */
	@Override
	public final Item item() {
		return item;
	}

	/**
	 * 処理の結果を説明する文字列を返します。
	 *
	 *
	 * @return 文字列
	 */
	@Override
	public final String text() {
		return text;
	}

	/**
	 * 総得点やマルチ計算に使用される識別子を返します。
	 *
	 *
	 * @param keyNum 識別子の配列内の位置
	 *
	 * @return 指定された位置にある識別子
	 *
	 * @throws IndexOutOfBoundsException 範囲外の場合
	 */
	@Override
	public final Object key(int keyNum) {
		throw new IndexOutOfBoundsException();
	}

	/**
	 * この交信に関連づけられた識別子の個数を返します。
	 *
	 *
	 * @return マルチの個数
	 */
	@Override
	public final int size() {
		return 0;
	}

	/**
	 * この交信の文字列による表現を返します。
	 *
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		final var joiner = new StringJoiner(" ", "{", "}");
		joiner.add(Failure.class.getCanonicalName());
		joiner.add(String.format("text:%s", text()));
		return joiner.toString();
	}
}
