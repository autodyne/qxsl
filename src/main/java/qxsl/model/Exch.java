/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
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
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/15
 *
 */
public abstract class Exch extends Tuple {
	/**
	 * 指定された要素名を持つ空のタプルを構築します。
	 * 
	 * @param name タプルの名前
	 */
	public Exch(QName name) {
		super(name);
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
