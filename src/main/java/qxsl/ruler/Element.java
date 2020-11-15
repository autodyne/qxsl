/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Serializable;
import java.util.Arrays;

/**
 * 得点計算で利用される要素またはその列です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/15
 */
public final class Element implements Serializable {
	private Object[] vals;

	/**
	 * 指定された値を要素とします。
	 *
	 *
	 * @param vals 要素の列
	 */
	public Element(Object...vals) {
		this.vals = vals;
	}

	/**
	 * この要素が含む要素の数を返します。
	 *
	 *
	 * @return 要素の個数
	 */
	public final int size() {
		return vals.length;
	}

	/**
	 * 指定された位置の要素を取得します。
	 *
	 *
	 * @param index 要素の位置
	 *
	 * @return 要素
	 */
	public final Element get(int index) {
		return new Element(vals[index]);
	}

	/**
	 * この要素のハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Arrays.hashCode(vals);
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
		if(!Element.class.isInstance(obj)) return false;
		return Arrays.equals(vals, ((Element) obj).vals);
	}
}
