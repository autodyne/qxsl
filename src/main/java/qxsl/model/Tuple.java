/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import qxsl.field.FieldFormat;
import qxsl.field.FieldFormats;

import javax.xml.namespace.QName;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;

/**
 * 交信に対して複数の属性を設定可能な交信記録の共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2015/08/05
 */
public abstract class Tuple implements Iterable<Field> {
	final Map<QName, Field> table;
	final QName qname;

	/**
	 * 指定された名前の空の要素を構築します。
	 *
	 * @param qname 名前
	 */
	public Tuple(QName qname) {
		this.qname = qname;
		this.table = new HashMap<>();
	}

	/**
	 * この要素の名前を返します。
	 *
	 * @return 要素の名前
	 */
	public final QName name() {
		return qname;
	}

	/**
	 * この要素のハッシュ値を計算します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public abstract int hashCode();

	/**
	 * 指定された要素と等値であるか確認します。
	 *
	 *
	 * @param obj 比較する要素
	 *
	 * @return 同じ情報を保持する要素は真
	 */
	@Override
	public abstract boolean equals(Object obj);

	/**
	 * この要素の属性を並べた反復子を返します。
	 *
	 * @return 反復子
	 */
	@Override
	public final Iterator<Field> iterator() {
		return table.values().iterator();
	}

	/**
	 * 指定された名前の属性の有無を確認します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性が設定されている場合は真
	 */
	public final boolean containsKey(QName key) {
		return table.containsKey(key);
	}

	/**
	 * 指定された属性を適切な名前で追加します。
	 *
	 *
	 * @param field 追加する属性
	 *
	 * @return この要素
	 */
	public final Tuple set(Field field) {
		table.put(field.name(), field);
		return this;
	}

	/**
	 * 指定された文字列を属性に変換して追加します。
	 *
	 *
	 * @param key 属性の名前
	 * @param val 属性値の文字列
	 *
	 * @return この要素
	 *
	 * @since 2019/06/30
	 */
	public final Tuple set(QName key, String val) {
		set(FieldFormats.FIELDS.decode(key, val));
		return this;
	}

	/**
	 * 指定された属性名に対応する属性を削除します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return この要素
	 */
	public final Tuple remove(QName key) {
		table.remove(key);
		return this;
	}

	/**
	 * 指定された属性名に対応する属性を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 設定されている属性
	 */
	public final Field get(QName key) {
		return table.get(key);
	}

	/**
	 * 指定された属性名に対応する属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 設定されている属性の値
	 */
	public final Object value(QName key) {
		return containsKey(key)? get(key).value(): null;
	}
}
