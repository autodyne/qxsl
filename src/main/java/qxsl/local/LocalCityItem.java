/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.local;

import java.io.Serializable;
import java.util.Arrays;

/**
 * 相手局または自局が運用している地域を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2014/04/20
 */
public final class LocalCityItem implements Serializable {
	private final String[] path;
	private final int size;

	/**
	 * 指定された地域を構築します。
	 *
	 *
	 * @param path 地域の絶対パス
	 */
	public LocalCityItem(String...path) {
		this.path = path;
		this.size = path.length - 1;
	}

	/**
	 * この地域を自然な文字列に変換します。
	 *
	 *
	 * @return 自然名
	 */
	@Override
	public final String toString() {
		return name();
	}

	/**
	 * この地域を特定する識別子を返します。
	 *
	 *
	 * @return 識別番号
	 */
	public final String code() {
		return path[path.length - 1];
	}

	/**
	 * この地域を含む最大の区画を返します。
	 *
	 *
	 * @return 最大の区画
	 */
	public final String area() {
		return name(0);
	}

	/**
	 * この地域を特定する自然名を返します。
	 *
	 *
	 * @return 平易な名前
	 */
	public final String name() {
		return name(size - 1);
	}

	/**
	 * 指定された階層までの名前を返します。
	 *
	 *
	 * @param lv 階層
	 *
	 * @return 平易な名前
	 *
	 * @throws IndexOutOfBoundsException 階層が深すぎる場合
	 */
	public final String name(int lv) {
		if(lv >= size) throw new IndexOutOfBoundsException();
		return String.join(" ", Arrays.copyOf(path, lv + 1));
	}

	/**
	 * この地域のハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 *
	 * @since 2023/05/03
	 */
	@Override
	public final int hashCode() {
		return Arrays.hashCode(path);
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 *
	 *
	 * @param obj 比較するオブジェクト
	 *
	 * @return この属性と等しい場合true
	 *
	 * @since 2023/05/03
	 */
	@Override
	public final boolean equals(Object obj) {
		if(obj instanceof LocalCityItem) {
			final var item = (LocalCityItem) obj;
			return Arrays.equals(path, item.path);
		} else return false;
	}
}
