/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;

import java.io.Serializable;
import java.util.StringJoiner;

/**
 * 規約により受理された交信に付与され総得点や乗数の計算に使用されます。
 * 総得点や乗数は識別子の異なり数で求めます。
 * 識別子は複数指定できますが、最初の識別子は交信の重複排除に使います。
 * 以降の識別子は乗数の計算に使用されます。乗数の個数は規約に依ります。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/26
 */
public final class Success implements Message, Serializable {
	private static final long serialVersionUID = 1L;
	private final Item item;
	private final int score;
	private final Object[] keys;

	/**
	 * 交信の実体と得点の計算に使用する識別子と得点を設定します。
	 *
	 * @param item 交信の実体
	 * @param score 得点
	 * @param keys 総得点や乗数の計算に使用する識別子
	 */
	public Success(Item item, int score, Object...keys) {
		this.score = score;
		this.item = item;
		this.keys = keys;
	}

	/**
	 * 交信の成立により得られる素点を返します。
	 *
	 * @return 交信1件の得点
	 */
	@Override
	public final int score() {
		return score;
	}

	/**
	 * 交信の実体を表す{@link Item}を返します。
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
	 * @return 文字列
	 */
	@Override
	public final String text() {
		return "";
	}

	/**
	 * 総得点や乗数の計算に使用される識別子を返します。
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
		return keys[keyNum];
	}

	/**
	 * この交信に関連づけられた識別子の個数を返します。
	 *
	 * @return 乗数の個数
	 */
	@Override
	public final int countKeys() {
		return keys.length;
	}

	/**
	 * この交信の文字列による表現を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		final var outer = new StringJoiner(" ", "{", "}");
		final var inner = new StringJoiner(",", "[", "]");
		outer.add(Success.class.getCanonicalName());
		outer.add(String.format("score:%d", score));
		for(var k: keys) inner.add(String.valueOf(k));
		return outer.add(String.format("keys:%s", inner)).toString();
	}
}
