/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.qxsl;

import javax.xml.namespace.QName;
import qxsl.extra.field.adif.Adif;
import qxsl.model.Field;
import qxsl.model.Tuple;

/**
 * QXMLが標準仕様でサポートする属性の名前を定義します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/27
 *
 * @param <V> 属性の属性値の総称型
 */
public abstract class Qxsl<V> extends Field<V> {
	public static final String NAMESPACE = "qxsl.org";
	public static final QName BAND = new QName(NAMESPACE, "band", "qxsl");
	public static final QName CALL = new QName(NAMESPACE, "call", "qxsl");
	public static final QName CODE = new QName(NAMESPACE, "code", "qxsl");
	public static final QName MODE = new QName(NAMESPACE, "mode", "qxsl");
	public static final QName NAME = new QName(NAMESPACE, "name", "qxsl");
	public static final QName NOTE = new QName(NAMESPACE, "note", "qxsl");
	public static final QName RSTQ = new QName(NAMESPACE, "rstq", "qxsl");
	public static final QName TIME = new QName(NAMESPACE, "time", "qxsl");
	public static final QName WATT = new QName(NAMESPACE, "watt", "qxsl");

	/**
	 * 指定した属性名を持つ属性を構築します。
	 * 
	 * @param name 属性名
	 */
	public Qxsl(QName name) {
		super(name);
	}
}
