/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.ruler;

import java.util.List;

import qxsl.local.LocalCityItem;
import qxsl.model.Item;

/**
 * 参加者が不参加の部門を表す特殊な実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/23
 */
public abstract class Absence extends Section {
	/**
	 * 部門を構築します。
	 */
	public Absence() {}

	/**
	 * 指定された交信記録の妥当性を検査します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 検証結果
	 */
	@Override
	public final Message verify(Item item) {
		return new Failure(item, "N/A");
	}

	/**
	 * 指定された交信記録の識別子を発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 重複を除くための識別子
	 *
	 * @since 2020/11/02
	 */
	@Override
	public final Element unique(Item item) {
		return new Element();
	}

	/**
	 * 指定された交信記録のマルチを発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 総得点を計算する識別子の配列
	 *
	 * @since 2020/11/02
	 */
	@Override
	public final Element entity(Item item) {
		return new Element();
	}

	/**
	 * 指定された集計結果の総得点を計算します。
	 *
	 *
	 * @param items 集計結果
	 *
	 * @return 総得点
	 *
	 * @since 2020/02/26
	 */
	@Override
	public final int result(Summary items) {
		return 0;
	}

	/**
	 * この部門に参加可能な運用場所を返します。
	 *
	 *
	 * @return 空のリスト
	 *
	 * @since 2022/06/22
	 */
	@Override
	public final List<LocalCityItem> getCityList() {
		return List.of();
	}

	/**
	 * 標準的な実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2024/10/19
	 */
	public static abstract class Default extends Absence {
		private final String name;
		private final String code;

		/**
		 * 指定された部門を構築します。
		 *
		 *
		 * @param name 部門の名前
		 * @param code 部門の分類
		 */
		public Default(String name, String code) {
			this.name = name;
			this.code = code;
		}

		@Override
		public final String name() {
			return name;
		}

		@Override
		public final String code() {
			return code;
		}
	}
}
