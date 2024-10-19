/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.ruler;

import java.time.LocalDate;
import java.time.Year;
import java.time.ZoneId;

/**
 * コンテストの日程はこのクラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/01
 */
public abstract class Program extends Contest {
	/**
	 * 指定された部門を有する規約を構築します。
	 *
	 *
	 * @param sections 部門の集合
	 */
	public Program(Section...sections) {
		super(sections);
	}

	/**
	 * コンテストの運営の名前を返します。
	 *
	 *
	 * @return 運営の名前
	 */
	public abstract String host();

	/**
	 * コンテストの運営の連絡先を返します。
	 *
	 *
	 * @return 運営の連絡先
	 */
	public abstract String mail();

	/**
	 * コンテストの規約の参照先を返します。
	 *
	 *
	 * @return 規約の参照先
	 */
	public abstract String link();

	/**
	 * 参加者を助ける文字列を返します。
	 *
	 *
	 * @return 助けとなる文字列
	 *
	 * @since 2022/08/01
	 */
	public abstract String help();

	/**
	 * コンテストの開催年を返します。
	 * 半年を過ぎると翌年を返します。
	 *
	 *
	 * @return 名前
	 *
	 * @since 2022/07/23
	 */
	public int year() {
		final int year = Year.now().getValue();
		final var date = this.getStartDay(year);
		final var past = date.until(LocalDate.now());
		return past.getMonths() >= 6? year + 1: year;
	}

	/**
	 * コンテストの開始日を返します。
	 *
	 *
	 * @return 開始日
	 */
	public final LocalDate getStartDay() {
		return getStartDay(year());
	}

	/**
	 * コンテストの終了日を返します。
	 *
	 *
	 * @return 終了日
	 */
	public final LocalDate getFinalDay() {
		return getFinalDay(year());
	}

	/**
	 * コンテストの締切日を返します。
	 *
	 *
	 * @return 締切日
	 */
	public final LocalDate getDeadLine() {
		return getDeadLine(year());
	}

	/**
	 * 指定された年のコンテストの開始日を計算します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 開始日
	 */
	public abstract LocalDate getStartDay(int year);

	/**
	 * 指定された年のコンテストの終了日を計算します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 終了日
	 */
	public abstract LocalDate getFinalDay(int year);

	/**
	 * 指定された年のコンテストの締切日を計算します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 締切日
	 */
	public abstract LocalDate getDeadLine(int year);

	/**
	 * 現時点で参加登録が受付可能か確認します。
	 *
	 *
	 * @return 現在時刻で受付可能な場合は真
	 */
	public final boolean accept() {
		return accept(ZoneId.systemDefault());
	}

	/**
	 * 現時点で集計結果が閲覧可能か確認します。
	 *
	 *
	 * @return 現在時刻で閲覧可能な場合は真
	 */
	public final boolean finish() {
		return finish(ZoneId.systemDefault());
	}

	/**
	 * 現時点で参加登録が受付可能か確認します。
	 *
	 *
	 * @param zone タイムゾーン
	 *
	 * @return 現在時刻で受付可能な場合は真
	 */
	public boolean accept(ZoneId zone) {
		final var dead = this.getDeadLine(year());
		return !LocalDate.now(zone).isAfter(dead);
	}

	/**
	 * 現時点で集計結果が閲覧可能か確認します。
	 *
	 *
	 * @param zone タイムゾーン
	 *
	 * @return 現在時刻で閲覧可能な場合は真
	 */
	public boolean finish(ZoneId zone) {
		final var dead = this.getDeadLine(year());
		return dead.isBefore(LocalDate.now(zone));
	}

	/**
	 * 複数の部門に登録可能な場合の限度を確認します。
	 *
	 *
	 * @param code 確認の対象となる部門の分類
	 *
	 * @return 登録可能な個数の限度
	 *
	 * @since 2022/07/17
	 */
	public abstract int limitMultipleEntry(String code);

	/**
	 * 指定された部門にまとめて登録可能か確認します。
	 *
	 *
	 * @param entries 参加を試みる部門の配列
	 *
	 * @return 規約に違反する場合は真
	 *
	 * @since 2022/07/17
	 */
	public abstract boolean conflict(Section[] entries);
}
