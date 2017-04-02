/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table.secret;

import javax.xml.namespace.QName;

import qxsl.table.TableFormat;

/**
 * 標準仕様でサポートする属性の名前を定義します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/10
 *
 */
public abstract class BaseFormat implements TableFormat {
	public static final String PREFIX = "qxsl";
	public static final String NSURI  = "qxsl.org";
	public static final QName  BAND = new QName(NSURI, "band", PREFIX);
	public static final QName  CALL = new QName(NSURI, "call", PREFIX);
	public static final QName  CITY = new QName(NSURI, "city", PREFIX);
	public static final QName  CODE = new QName(NSURI, "code", PREFIX);
	public static final QName  MODE = new QName(NSURI, "mode", PREFIX);
	public static final QName  NAME = new QName(NSURI, "name", PREFIX);
	public static final QName  NOTE = new QName(NSURI, "note", PREFIX);
	public static final QName  RSTQ = new QName(NSURI, "rstq", PREFIX);
	public static final QName  TIME = new QName(NSURI, "time", PREFIX);
	public static final QName  WATT = new QName(NSURI, "watt", PREFIX);
}
