/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.model;

import java.util.Collections;
import java.util.Iterator;
import java.util.StringJoiner;
import javax.xml.namespace.QName;

/**
 * 交信時に相手局と交換した情報を表現する{@link Tuple}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/15
 *
 */
public abstract class Exch extends Tuple {
	private final Item item;

	/**
	 * 指定された要素名と親を持つ空のタプルを構築します。
	 *
	 * @param item 親となる要素
	 * @param name タプルの名前
	 */
	protected Exch(Item item, QName name) {
		super(name);
		this.item = item;
	}

	/**
	 * このタプルの親である交信記録を返します。
	 *
	 * @return 交信記録
	 */
	public final Item getItem() {
		return item;
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
		return String.format("{%s}", sj);
	}

	@Override
	public final Iterator<Tuple> children() {
		return Collections.emptyIterator();
	}
}
