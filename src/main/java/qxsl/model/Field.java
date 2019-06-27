/*****************************************************************************
 * Amateur Radio Operational Logging Library 'xsum' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import java.util.Objects;
import javax.xml.namespace.QName;

/**
 * {@link Tuple}に付随する各種の属性は{@link Field}クラスを実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2015/08/05
 * 
 * @param <V> 属性の属性値の総称型
 */
public abstract class Field<V> {
	private final QName name;

	/**
	 * 指定した属性名を持つ属性を構築します。
	 * 
	 * @param name 属性名
	 */
	public Field(QName name) {
		this.name = name;
	}

	/**
	 * この属性の名前を返します。
	 *
	 * @return 属性の名前
	 */
	public QName name() {
		return name;
	}

	/**
	 * この属性の値を返します。
	 * 
	 * @return 属性の値
	 */
	public abstract V value();

	/**
	 * {@link #value()}の返り値を文字列で返します。
	 * 
	 * @return 文字列
	 */
	@Override
	public String toString() {
		return String.valueOf(value());
	}

	/**
	 * {@link #value()}の返り値をハッシュ値にして返します。
	 * 
	 * @return ハッシュ値
	 */
	@Override
	public int hashCode() {
		return Objects.hash(name(), value());
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 * 対象のオブジェクトが{@link Field}であり、
	 * 双方の{@link #value()}の返り値が等価な場合、
	 * trueを返します。
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

	/**
	 * この属性をADIFの名前空間の属性に変換します。
	 * デフォルトでこのメソッドは変換を行いません。
	 *
	 * @param tuple この属性を格納しているタプル
	 *
	 * @since 2019/06/27
	 */
	public void toADIF(Tuple tuple) {}

	/**
	 * この属性をQXSLの名前空間の属性に変換します。
	 * デフォルトでこのメソッドは変換を行いません。
	 *
	 * @param tuple この属性を格納しているタプル
	 *
	 * @since 2019/06/27
	 */
	public void toQXSL(Tuple tuple) {}
}
