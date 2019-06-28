/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.adif;

import javax.xml.namespace.QName;
import qxsl.extra.field.qxsl.Qxsl;
import qxsl.model.Field;
import qxsl.model.Tuple;

/**
 * ADIFのサブセット仕様でサポートする属性の名前を定義します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/27
 *
 * @param <V> 属性の属性値の総称型
 */
public abstract class Adif<V> extends Field<V> {
	public static final QName BAND = new QName("adif.org", "BAND", "adif");
	public static final QName CALL = new QName("adif.org", "CALL", "adif");
	public static final QName MODE = new QName("adif.org", "MODE", "adif");
	public static final QName TIME = new QName("adif.org", "TIME_ON",  "adif");
	public static final QName DATE = new QName("adif.org", "QSO_DATE", "adif");
	public static final QName RRST = new QName("adif.org", "RST_RCVD", "adif");
	public static final QName SRST = new QName("adif.org", "RST_SENT", "adif");
	public static final QName RNUM = new QName("adif.org", "RTX", "adif");
	public static final QName SNUM = new QName("adif.org", "STX", "adif");

	/**
	 * 指定した属性名を持つ属性を構築します。
	 * 
	 * @param name 属性名
	 */
	public Adif(QName name) {
		super(name);
	}
}
