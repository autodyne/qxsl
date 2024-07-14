/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.utils;

import java.util.Comparator;
import java.util.function.ToIntBiFunction;

/**
 * レーベンシュタイン距離の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2024/07/14
 */
public final class LevenDist implements ToIntBiFunction<String, String> {
	/**
	 * 指定された文字列と文字列の間の編集距離を計算します。
	 *
	 *
	 * @param str1 文字列
	 * @param str2 文字列
	 *
	 * @return 編集距離
	 */
	@Override
	public final int applyAsInt(String str1, String str2) {
		final int rows = str1.length() + 1;
		final int cols = str2.length() + 1;
		final var edit = new int[rows][cols];
		for(int row = 0; row < rows; row++) edit[row][0] = row;
		for(int col = 0; col < cols; col++) edit[0][col] = col;
		for(int row = 1; row < rows; row++) {
			for(int col = 1; col < cols; col++) {
				final char c1 = str1.charAt(row - 1);
				final char c2 = str2.charAt(col - 1);
				final int pre = edit[row - 1][col - 1];
				final int add = edit[row][col - 1] + 1;
				final int del = edit[row - 1][col] + 1;
				final int rep = c1 == c2? pre: pre + 1;
				edit[row][col] = Math.min(rep, Math.min(add, del));
			}
		}
		return edit[rows - 1][cols - 1];
	}

	/**
	 * 指定された文字列との距離で文字列の順序を定義します。
	 *
	 *
	 * @param anchor 文字列
	 *
	 * @return 順序の定義
	 */
	public static final Comparator<String> comparator(String anchor) {
		final var leven = new LevenDist();
		return (str1, str2) -> {
			final int d1 = leven.applyAsInt(anchor, str1);
			final int d2 = leven.applyAsInt(anchor, str2);
			return Integer.compare(d1, d2);
		};
	}
}
