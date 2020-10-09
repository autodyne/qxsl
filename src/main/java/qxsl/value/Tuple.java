/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.value;

import java.util.Iterator;
import javax.xml.namespace.QName;

/**
 * 交信に対して複数の属性を設定可能な交信記録の共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2015/08/05
 */
public abstract class Tuple implements Iterable<Field> {
	private final QName name;

	/**
	 * 指定された名前の要素を構築します。
	 *
	 *
	 * @param name 要素の名前
	 */
	public Tuple(QName name) {
		this.name = name;
	}

	/**
	 * この要素の名前を返します。
	 *
	 *
	 * @return 要素の名前
	 */
	public final QName name() {
		return name;
	}

	/**
	 * この要素のハッシュ値を計算します。
	 *
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
	 *
	 * @return 反復子
	 */
	@Override
	public abstract Iterator<Field> iterator();

	/**
	 * 指定された名前の属性の有無を確認します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性が設定されている場合は真
	 */
	public abstract boolean containsKey(QName key);

	/**
	 * 指定された属性を適切な名前で追加します。
	 *
	 *
	 * @param field 追加する属性
	 *
	 * @return この要素
	 */
	public abstract Tuple set(Field field);

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
	public abstract Tuple set(QName key, String val);

	/**
	 * 指定された属性名に対応する属性を削除します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return この要素
	 */
	public abstract Tuple remove(QName key);

	/**
	 * 指定された属性名に対応する属性を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 設定されている属性
	 */
	public abstract Field get(QName key);

	/**
	 * 指定された属性名に対応する属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 設定されている属性の値
	 */
	public abstract Object value(QName key);
}
