/*****************************************************************************
 * Amateur Radio Operational Logging Library 'xsum' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import javax.xml.namespace.QName;

/**
 * 属性として複数の{@link Field}を設定可能なデータ構造です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2015/08/05
 *
 */
public abstract class Tuple<T extends Tuple<T>> implements Iterable<Field> {
	private final QName type;
	private final Map<QName, Field> table;

	/**
	 * 指定した種別の空のタプルを構築します。
	 * 
	 * @param type 種別
	 */
	public Tuple(QName type) {
		this.type = type;
		table = new HashMap<>();
	}

	/**
	 * このタプルの名前を返します。
	 *
	 * @return タプルの名前
	 */
	public final QName type() {
		return type;
	}

	/**
	 * このタプルのハッシュ値を計算します。
	 * 
	 * @return ハッシュ値
	 */
	@Override
	public int hashCode() {
		int val = Objects.hash(type(), table);
		if(this instanceof Item) {
			final Rcvd rcvd = ((Item) this).getRcvd();
			final Sent sent = ((Item) this).getSent();
			val = val * 31 + Objects.hash(rcvd, sent);
		}
		return val;
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 * 
	 * @param obj 比較するオブジェクト
	 * @return このタプルと等しい場合true
	 */
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof Tuple)) return false;
		return table.equals(((Tuple) obj).table);
	}

	/**
	 * 全ての属性値を要素に持つ{@link Iterator}を返します。
	 * 
	 * @return 全ての属性値を列挙した反復子
	 */
	@Override
	public final Iterator<Field> iterator() {
		return table.values().iterator();
	}

	/**
	 * 指定した{@link QName}に対応する属性を返します。
	 * 
	 * @param name 属性の名前
	 * @return 設定されている属性
	 */
	public final Field get(QName name) {
		return table.get(name);
	}

	/**
	 * 指定した{@link Field}クラスに対応する属性を返します。
	 * 
	 * @param <F> 属性の総称型
	 * @param field 属性のクラス
	 * @return 設定されている属性
	 */
	public final <F extends Field> F get(Class<F> field) {
		for(Field f: this) if(field.equals(f.getClass())) {
			@SuppressWarnings ("unchecked")
			final F found = (F) f;
			return found;
		}
		return null;
	}

	/**
	 * 指定した{@link Field}クラスに対応する属性の値を返します。
	 * 
	 * @param <F> 属性の総称型
	 * @param <V> 属性の値の総称型
	 * @param field 属性のクラス
	 * @return 設定されている属性の値
	 */
	public final <F extends Field<V>, V> V value(Class<F> field) {
		return get(field) != null? get(field).value() : null;
	}

	/**
	 * 指定した{@link QName}に対応する属性を削除します。
	 * 
	 * @param qname 属性の名前
	 * @return このタプル
	 */
	public final T remove(QName qname) {
		table.remove(qname);
		@SuppressWarnings("unchecked")
		T thisNode = (T) this;
		return thisNode;
	}

	/**
	 * 指定した属性をこのタプルに設定します。
	 * 
	 * @param field 設定する属性
	 * @return このタプル
	 * @throws NullPointerException 属性がnullの場合
	 */
	public final T set(Field field) throws NullPointerException {
		table.put(field.type(), field);
		@SuppressWarnings("unchecked")
		T thisNode = (T) this;
		return thisNode;
	}
}
