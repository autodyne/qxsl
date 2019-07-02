/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field;

import javax.xml.namespace.QName;
import qxsl.model.Field;

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
	public static final String QXSL = "qxsl.org";
	public static final String ADIF = "adif.org";
	public static final QName BAND = new QName(QXSL, "band", "qxsl");
	public static final QName CALL = new QName(QXSL, "call", "qxsl");
	public static final QName CITY = new QName(QXSL, "city", "qxsl");
	public static final QName CODE = new QName(QXSL, "code", "qxsl");
	public static final QName MODE = new QName(QXSL, "mode", "qxsl");
	public static final QName NAME = new QName(QXSL, "name", "qxsl");
	public static final QName NOTE = new QName(QXSL, "note", "qxsl");
	public static final QName RSTQ = new QName(QXSL, "rstq", "qxsl");
	public static final QName TIME = new QName(QXSL, "time", "qxsl");
	public static final QName WATT = new QName(QXSL, "watt", "qxsl");

	/**
	 * 指定した属性名を持つ属性を構築します。
	 * 
	 * @param name 属性名
	 */
	public Qxsl(QName name) {
		super(name);
	}
}