/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
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
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/04
 *
 */
public final class Item extends Tuple {
	private Rcvd rcvd;
	private Sent sent;

	/**
	 * 空のタプルを構築します。
	 */
	public Item() {
		super(ITEM);
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 * 
	 * @param obj 比較するオブジェクト
	 * @return このタプルと等しい場合true
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
	 * このタプルの文字列による表現を返します。
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
	 * タプル直下にある{@link Rcvd}を返します。
	 * 
	 * @return {@link Rcvd}
	 */
	public Rcvd getRcvd() {
		if(rcvd == null) rcvd = new Rcvd(this);
		return rcvd;
	}

	/**
	 * タプル直下にある{@link Sent}を返します。
	 * 
	 * @return {@link Sent}
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
