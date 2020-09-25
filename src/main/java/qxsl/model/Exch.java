/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import java.util.StringJoiner;
import javax.xml.namespace.QName;

/**
 * 交信時に相手局と交換した情報を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/15
 */
public abstract class Exch extends Tuple {
	private final Item item;

	/**
	 * 指定された名前と親を持つ空の要素を構築します。
	 *
	 *
	 * @param item 親となる要素
	 * @param name タプルの名前
	 */
	protected Exch(Item item, QName name) {
		super(name);
		this.item = item;
	}

	/**
	 * この要素の親である交信記録を返します。
	 *
	 *
	 * @return 交信記録
	 */
	public final Item getItem() {
		return item;
	}

	/**
	 * この要素のハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return table.hashCode();
	}

	/**
	 * 指定された要素と等値であるか確認します。
	 *
	 *
	 * @param obj 比較する要素
	 *
	 * @return 同じ情報を保持する要素は真
	 */
	@Override
	public final boolean equals(Object obj) {
		if(!getClass().isInstance(obj)) return false;
		else return table.equals(((Exch) obj).table);
	}

	/**
	 * この交信記録の文字列による表現を返します。
	 *
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		final var join = new StringJoiner(" ");
		for(var v: this) join.add(v.value().toString());
		return String.format("{%s: %s}", name(), join);
	}
}
