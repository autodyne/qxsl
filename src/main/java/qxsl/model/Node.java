/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.xml.namespace.QName;

import qxsl.field.FieldManager;
import qxsl.value.Field;
import qxsl.value.Tuple;

/**
 * 交信記録の末端の要素を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/15
 */
public abstract class Node extends Tuple {
	private final Map<QName, Field> table;

	/**
	 * 指定された名前の要素を構築します。
	 *
	 *
	 * @param name 要素の名前
	 */
	public Node(QName name) {
		super(name);
		this.table = new HashMap<>();
	}

	/**
	 * この要素のハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return table.hashCode();
	}

	/**
	 * 指定された要素と等値であるか確認します。
	 *
	 *
	 * @param obj 比較する要素
	 *
	 * @return 同じ情報を保持する要素は真
	 */
	@Override
	public final boolean equals(Object obj) {
		if(!getClass().isInstance(obj)) return false;
		else return table.equals(((Node) obj).table);
	}

	/**
	 * この交信記録の文字列による表現を返します。
	 *
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		return String.format("%s=%s", name(), table);
	}

	/**
	 * この要素の属性を並べた反復子を返します。
	 *
	 *
	 * @return 反復子
	 */
	@Override
	public final Iterator<Field> iterator() {
		return this.table.values().iterator();
	}

	/**
	 * 指定された名前の属性の有無を確認します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性が設定されている場合は真
	 */
	@Override
	public final boolean containsKey(QName key) {
		return this.table.containsKey(key);
	}

	/**
	 * 指定された属性を適切な名前で追加します。
	 *
	 *
	 * @param field 追加する属性
	 *
	 * @return この要素
	 */
	@Override
	public final Tuple set(Field field) {
		this.table.put(field.name(), field);
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
	@Override
	public final Tuple set(QName key, Object val) {
		FieldManager.FIELDS.set(this, key, val);
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
	@Override
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
	@Override
	public final Field get(QName key) {
		return table.get(key);
	}

	/**
	 * 指定された属性名に対応する属性を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性なき場合は空の属性
	 */
	public final Field some(QName key) {
		if(containsKey(key)) return table.get(key);
		return FieldManager.FIELDS.decode(key, "");
	}

	/**
	 * 指定された属性名に対応する属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 設定されている属性の値
	 */
	@Override
	public final Object value(QName key) {
		return containsKey(key)? get(key).value(): null;
	}
}
