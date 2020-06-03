/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import java.util.Objects;
import javax.xml.namespace.QName;
import qxsl.field.FieldFormats.Any;

/**
 * {@link Tuple}に付随する各種の属性は{@link Field}クラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2015/08/05
 *
 * @param <V> 属性の属性値の総称型
 */
public abstract class Field<V> {
	private final QName qname;

	/**
	 * 指定された属性名を持つ属性を構築します。
	 *
	 * @param qname 属性名
	 */
	public Field(QName qname) {
		this.qname = qname;
	}

	/**
	 * この属性の名前を返します。
	 *
	 * @return 属性の名前
	 */
	public QName name() {
		return qname;
	}

	/**
	 * この属性の値を返します。
	 *
	 * @return 属性の値
	 */
	public abstract V value();

	/**
	 * この属性が{@link Any}か確認します。
	 *
	 * @return Anyの場合は真
	 */
	public final boolean isAny() {
		return this instanceof Any;
	}

	/**
	 * 属性値を文字列で返します。
	 *
	 * @return 文字列
	 */
	@Override
	public String toString() {
		return String.valueOf(value());
	}

	/**
	 * 属性値のハッシュ値を計算します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public int hashCode() {
		return Objects.hash(name(), value());
	}

	/**
	 * 指定されたオブジェクトと等値であるかを確認します。
	 * 同じ値を持つ同じクラスの属性の場合に真を返します。
	 *
	 * @param obj 比較するオブジェクト
	 * @return この属性と等しい場合true
	 */
	@Override
	public boolean equals(Object obj) {
		if(getClass().isInstance(obj)) {
			Field nobj = (Field) obj;
			Object mine = this.value();
			Object your = nobj.value();
			return mine.equals(your);
		}
		return false;
	}
}
