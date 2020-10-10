/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import java.util.Iterator;
import java.util.Objects;
import java.util.StringJoiner;
import javax.xml.namespace.QName;

import qxsl.value.Field;
import qxsl.value.Tuple;

import static gaas.table.QxmlFactory.ITEM;

/**
 * 交信記録で不可分な交信を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/04
 */
public final class Item extends Tuple {
	private final Both both;
	private final Rcvd rcvd;
	private final Sent sent;

	/**
	 * 交信記録を構築します。
	 */
	public Item() {
		super(ITEM);
		this.both = new Both();
		this.rcvd = new Rcvd();
		this.sent = new Sent();
	}

	/**
	 * 送受信局間に共通の要素を返します。
	 *
	 *
	 * @return 送受信局間に共通の要素
	 */
	public final Both getBoth() {
		return both;
	}

	/**
	 * 相手局から受信した要素を返します。
	 *
	 *
	 * @return 相手局から受信した要素
	 */
	public final Rcvd getRcvd() {
		return rcvd;
	}

	/**
	 * 相手局まで送信した要素を返します。
	 *
	 *
	 * @return 相手局まで送信した要素
	 */
	public final Sent getSent() {
		return sent;
	}

	/**
	 * この要素のハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Objects.hash(both, rcvd, sent);
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
			if(!both.equals(item.both)) return false;
			if(!rcvd.equals(item.rcvd)) return false;
			if(!sent.equals(item.sent)) return false;
			return true;
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
		final var join = new StringJoiner(",");
		join.add(both.toString());
		join.add(rcvd.toString());
		join.add(sent.toString());
		return String.format("%s={%s}", name(), join);
	}

	/**
	 * この要素の属性を並べた反復子を返します。
	 *
	 *
	 * @return 反復子
	 */
	@Override
	public final Iterator<Field> iterator() {
		return getBoth().iterator();
	}

	/**
	 * 指定された名前の属性の有無を確認します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性が設定されている場合は真
	 */
	@Override
	public final boolean containsKey(QName key) {
		return getBoth().containsKey(key);
	}

	/**
	 * 指定された属性を適切な名前で追加します。
	 *
	 *
	 * @param field 追加する属性
	 *
	 * @return この要素
	 */
	@Override
	public final Tuple set(Field field) {
		return getBoth().set(field);
	}

	/**
	 * 指定された文字列を属性に変換して追加します。
	 *
	 *
	 * @param key 属性の名前
	 * @param val 属性値の文字列
	 *
	 * @return この要素
	 *
	 * @since 2019/06/30
	 */
	@Override
	public final Tuple set(QName key, String val) {
		return getBoth().set(key, val);
	}

	/**
	 * 指定された属性名に対応する属性を削除します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return この要素
	 */
	@Override
	public final Tuple remove(QName key) {
		return getBoth().remove(key);
	}

	/**
	 * 指定された属性名に対応する属性を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 設定されている属性
	 */
	@Override
	public final Field get(QName key) {
		return getBoth().get(key);
	}

	/**
	 * 指定された属性名に対応する属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 設定されている属性の値
	 */
	@Override
	public final Object value(QName key) {
		return getBoth().value(key);
	}
}
