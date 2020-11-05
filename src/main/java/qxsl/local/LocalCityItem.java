/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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
	 * @param values 地域の詳細を表す文字列
	 */
	protected LocalCityItem(String values) {
		final var strm = Arrays.stream(values.split(" +"));
		this.path = strm.distinct().toArray(String[]::new);
		this.size = this.path.length - 1;
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
}
