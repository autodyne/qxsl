/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import javax.xml.namespace.QName;

import qxsl.value.Field;

/**
 * QXMLが標準仕様でサポートする属性の名前を定義します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/27
 *
 * @param <V> 属性の属性値の総称型
 */
public abstract class Qxsl<V> extends Field<V> {
	final V value;

	/**
	 * 指定した名前と値の属性を構築します。
	 *
	 *
	 * @param qname 属性の名前
	 * @param value 属性の値
	 */
	public Qxsl(QName qname, V value) {
		super(qname);
		this.value = value;
	}

	/**
	 * この属性の値を返します。
	 *
	 *
	 * @return 属性の値
	 */
	@Override
	public final V value() {
		return value;
	}

	/**
	 * 標準的な属性に共通する名前空間です。
	 */
	public static final String URI = "qxsl.org";

	/**
	 * 標準的な属性に共通する接頭辞です。
	 */
	public static final String PRE = "qxsl";

	/**
	 * 交信の周波数帯を表す属性の名前です。
	 */
	public static final QName BAND = new QName(URI, "band", PRE);

	/**
	 * 相手の呼出符号を表す属性の名前です。
	 */
	public static final QName CALL = new QName(URI, "call", PRE);

	/**
	 * 交信の交換番号を表す属性の名前です。
	 */
	public static final QName CODE = new QName(URI, "code", PRE);

	/**
	 * 交信の通信方式を表す属性の名前です。
	 */
	public static final QName MODE = new QName(URI, "mode", PRE);

	/**
	 * 交信の獲得番号を表す属性の名前です。
	 */
	public static final QName MUL1 = new QName(URI, "mul1", PRE);

	/**
	 * 更なる獲得番号を表す属性の名前です。
	 */
	public static final QName MUL2 = new QName(URI, "mul2", PRE);

	/**
	 * 運用者の個人名を表す属性の名前です。
	 */
	public static final QName NAME = new QName(URI, "name", PRE);

	/**
	 * 交信記録の備考を表す属性の名前です。
	 */
	public static final QName NOTE = new QName(URI, "note", PRE);

	/**
	 * 交信のレポートを表す属性の名前です。
	 */
	public static final QName RSTQ = new QName(URI, "rstq", PRE);

	/**
	 * 交信の現地時刻を表す属性の名前です。
	 */
	public static final QName TIME = new QName(URI, "time", PRE);

	/**
	 * 交信の送信電力を表す属性の名前です。
	 */
	public static final QName WATT = new QName(URI, "watt", PRE);
}
