/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import javax.xml.namespace.QName;

import qxsl.field.FieldFormat;
import qxsl.field.FieldFormats;

/**
 * {@link Item}クラスや{@link Exch}クラスはこのクラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2015/08/05
 */
public abstract class Tuple implements Iterable<Field> {
	private final Map<QName, Field> table;
	private final QName qname;

	/**
	 * 指定された名前の空の要素を構築します。
	 *
	 * @param qname 名前
	 */
	public Tuple(QName qname) {
		this.qname = qname;
		table = new HashMap<>();
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
	public int hashCode() {
		int val = Objects.hash(name(), table);
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
	 * @return この要素と等しい場合true
	 */
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof Tuple)) return false;
		return table.equals(((Tuple) obj).table);
	}

	/**
	 * この要素に設定された全ての属性をイテレータで反復します。
	 * 隷下の要素に設定された属性はイテレーターに含まれません。
	 *
	 * @return 全ての属性を列挙したイテレータ
	 */
	@Override
	public final Iterator<Field> iterator() {
		return table.values().iterator();
	}

	/**
	 * この要素の隷下にある全ての要素をイテレータで返します。
	 *
	 * @return 全ての要素を列挙した反復子
	 */
	public abstract Iterator<Tuple> children();

	/**
	 * 指定された属性をこの要素の隷下に追加します。
	 *
	 * @param field 追加する属性
	 * @return この要素
	 * @throws NullPointerException 属性がnullの場合
	 */
	public final Tuple add(Field field) {
		table.put(field.name(), field);
		return this;
	}

	/**
	 * 指定された属性をこの要素の隷下に設定します。
	 * 属性がnullの場合、その属性が削除されます。
	 *
	 * @param qname 属性の名前
	 * @param field 対象の属性
	 * @return この要素
	 *
	 * @throws NullPointerException 属性名がnullの場合
	 * @throws IllegalArgumentException 属性名と属性が異なる場合
	 *
	 * @since 2019/06/30
	 */
	public final Tuple set(QName qname, Field field) {
		if(field == null) return remove(qname);
		if(qname.equals(field.name())) return add(field);
		String fmt = "field (%s)'s name must be '%s'";
		String msg = String.format(fmt, field, qname);
		throw new IllegalArgumentException(msg);
	}

	/**
	 * 指定された文字列を属性に変換して追加します。
	 * 文字列がnullの場合、その属性が削除されます。
	 *
	 * @param qname 属性の名前
	 * @param value 属性値の文字列
	 * @return この要素
	 *
	 * @throws NullPointerException 属性名がnullの場合
	 * @throws IllegalArgumentException 属性名が未知の場合
	 *
	 * @since 2019/06/30
	 */
	public final Tuple set(QName qname, String value) {
		if(value == null) return remove(qname);
		final FieldFormats formats = new FieldFormats();
		final FieldFormat fmt = formats.forName(qname);
		if(fmt != null) return set(qname, fmt.decode(value));
		throw new IllegalArgumentException(qname.toString());
	}

	/**
	 * 指定された属性名に対応する属性を削除します。
	 *
	 * @param qname 属性の名前
	 * @return この要素
	 */
	public final Tuple remove(QName qname) {
		table.remove(qname);
		return this;
	}

	/**
	 * 指定された属性名に対応する属性を返します。
	 *
	 * @param qname 属性の名前
	 * @return 設定されている属性
	 */
	public final Field get(QName qname) {
		return table.get(qname);
	}

	/**
	 * 指定された属性名に対応する属性の値を返します。
	 *
	 * @param qname 属性の名前
	 * @return 設定されている属性の値
	 */
	public final Object value(QName qname) {
		final Field field = this.get(qname);
		return field != null? field.value(): null;
	}
}
