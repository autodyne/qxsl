/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import java.util.Iterator;
import java.util.StringJoiner;
import java.util.stream.Stream;
import javax.xml.namespace.QName;

import static qxsl.extra.table.QxmlFormat.ITEM;

/**
 * 交信記録で1件の不可分な交信を表現する{@link Tuple}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/04
 */
public final class Item extends Tuple {
	private Rcvd rcvd;
	private Sent sent;

	/**
	 * 空の交信記録を構築します。
	 */
	public Item() {
		super(ITEM);
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 *
	 * @param obj 比較するオブジェクト
	 * @return この交信記録と等しい場合true
	 */
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof Item)) return false;
		final Item comp = (Item) obj;
		if(!getRcvd().equals(comp.getRcvd())) return false;
		if(!getSent().equals(comp.getSent())) return false;
		return super.equals(comp);
	}

	/**
	 * この交信記録の文字列による表現を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public String toString() {
		StringJoiner sj = new StringJoiner(" ");
		sj.add(name().getLocalPart());
		for(Field f: this) sj.add(f.toString());
		sj.add(getRcvd().toString());
		sj.add(getSent().toString());
		return String.format("{%s}", sj);
	}

	/**
	 * この交信記録の直下にある{@link Rcvd}を返します。
	 *
	 * @return 相手局から受信した情報
	 */
	public Rcvd getRcvd() {
		if(rcvd == null) rcvd = new Rcvd(this);
		return rcvd;
	}

	/**
	 * この交信記録の直下にある{@link Sent}を返します。
	 *
	 * @return 相手局まで送信した情報
	 */
	public Sent getSent() {
		if(sent == null) sent = new Sent(this);
		return sent;
	}

	@Override
	public final Iterator<Tuple> children() {
		return Stream.<Tuple>of(getRcvd(), getSent()).iterator();
	}
}
