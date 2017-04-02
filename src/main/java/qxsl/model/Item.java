/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import java.util.StringJoiner;
import javax.xml.namespace.QName;
import qxsl.field.*;
import qxsl.table.secret.QxmlFormat;

/**
 * 交信記録シートにおいて1回の不可分な交信を表現するタプルです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/04
 *
 */
public final class Item extends Tuple<Item> {
	private Rcvd rcvd;
	private Sent sent;

	/**
	 * 空のタプルを構築します。
	 */
	public Item() {
		super(QxmlFormat.ITEM);
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
		sj.add(type().getLocalPart());
		Fields fields = new Fields();
		for(Field f: this) sj.add(f.toString());
		sj.add(getRcvd().toString());
		sj.add(getSent().toString());
		return String.format("{%s}", sj);
	}

	/**
	 * タプル直下にある{@link Rcvd}を返します。
	 * 
	 * @return {@link Rcvd} 存在しない場合は生成される
	 */
	public Rcvd getRcvd() {
		if(rcvd == null) setRcvd(new Rcvd());
		return rcvd;
	}

	/**
	 * タプル直下に{@link Rcvd}を追加します。
	 * 
	 * @param rcvd 追加する子
	 */
	public void setRcvd(Rcvd rcvd) {
		this.rcvd = rcvd;
	}

	/**
	 * タプル直下にある{@link Sent}を返します。
	 * 
	 * @return {@link Sent} 存在しない場合は生成される
	 */
	public Sent getSent() {
		if(sent == null) sent = new Sent();
		return sent;
	}

	/**
	 * タプル直下に{@link Sent}を追加します。
	 * 
	 * @param sent 追加する子
	 */
	public void setSent(Sent sent) {
		this.sent = sent;
	}
}
