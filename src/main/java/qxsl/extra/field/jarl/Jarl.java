/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.jarl;

import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.model.Tuple;

/**
 * JARLの規定に基づく日本国内向けの属性の名前を定義します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/27
 *
 * @param <V> 属性の属性値の総称型
 */
public abstract class Jarl<V> extends Field<V> {
	public static final String NAMESPACE = "jarl.org";
	public static final QName CITY = new QName(NAMESPACE, "city", "jarl");

	/**
	 * 指定した属性名を持つ属性を構築します。
	 * 
	 * @param name 属性名
	 */
	public Jarl(QName name) {
		super(name);
	}
}
