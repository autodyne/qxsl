/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.time.LocalDate;
import java.util.Iterator;

/**
 * コンテストの規約の実装は{@link Contest}クラスを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Contest implements Iterable<Section> {
	private final String name;

	/**
	 * 指定された名前で規約を構築します。
	 *
	 * @param name コンテストの名前
	 */
	public Contest(String name) {
		this.name = name;
	}

	/**
	 * コンテストの名前を返します。
	 *
	 * @return コンテストの名前
	 */
	public final String getName() {
		return name;
	}

	/**
	 * UIで表示するためにコンテスト名を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * 指定された年の開催期間の開始日を返します。
	 *
	 * @param year 年
	 * @return 開催日 未設定の場合はnull
	 *
	 * @since 2020/03/07
	 */
	public abstract LocalDate getStartingDate(int year);

	/**
	 * 指定された年の参加登録の締切日を返します。
	 *
	 * @param year 年
	 * @return 締切日 未設定の場合はnull
	 *
	 * @since 2020/03/07
	 */
	public abstract LocalDate getDeadLineDate(int year);

	/**
	 * このコンテストの部門をイテレータで返します。
	 *
	 * @return 全ての部門を含むイテレータ
	 */
	public abstract Iterator<Section> iterator();

	/**
	 * 指定された名前の部門を返します。
	 *
	 * @param name 部門の名前
	 * @return 該当する部門 見つからない場合はnull
	 */
	public final Section getSection(String name) {
		for(Section sec: this) {
			if(sec.getName().equals(name)) return sec;
		}
		return null;
	}

	/**
	 * 指定された交信記録の総得点を計算します。
	 *
	 * @param sum 交信記録
	 * @return 総得点
	 *
	 * @since 2020/02/26
	 */
	public abstract int score(Summary sum);
}
