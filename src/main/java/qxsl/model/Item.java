/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import java.util.Objects;
import java.util.StringJoiner;

import static gaas.table.QxmlFactory.ITEM;

/**
 * 交信記録で1件の不可分な交信を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/04
 */
public final class Item extends Tuple {
	private final Rcvd rcvd;
	private final Sent sent;

	/**
	 * 交信記録を構築します。
	 */
	public Item() {
		super(ITEM);
		this.rcvd = new Rcvd(this);
		this.sent = new Sent(this);
	}

	/**
	 * この交信記録の{@link Rcvd}を返します。
	 *
	 *
	 * @return 相手局から受信した情報
	 */
	public final Rcvd getRcvd() {
		return rcvd;
	}

	/**
	 * この交信記録の{@link Sent}を返します。
	 *
	 *
	 * @return 相手局まで送信した情報
	 */
	public final Sent getSent() {
		return sent;
	}

	/**
	 * この要素を複製します。
	 *
	 *
	 * @return 複製
	 */
	@Override
	public final Item clone() {
		final Item item = new Item();
		item.table.putAll(this.table);
		item.rcvd.table.putAll(this.rcvd.table);
		item.sent.table.putAll(this.sent.table);
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
		return Objects.hash(table, rcvd, sent);
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
		if(Item.class.isInstance(obj)) {
			final var item = (Item) obj;
			if(!rcvd.equals(item.rcvd)) return false;
			if(!sent.equals(item.sent)) return false;
			return table.equals(item.table);
		} else return false;
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
		join.add(rcvd.toString());
		join.add(sent.toString());
		return String.format("{%s: %s}", name(), join);
	}
}
