/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.time.LocalDate;
import java.time.Year;
import java.time.ZoneId;
import java.util.*;

/**
 * コンテストの規約はこのクラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Contest extends Library implements Iterable<Section> {
	private final Map<String, Section> map;

	/**
	 * 指定された部門を有する規約を構築します。
	 *
	 *
	 * @param sections 部門の集合
	 */
	public Contest(Section...sections) {
		this.map = new LinkedHashMap<>();
		for(var s: sections) this.add(s);
	}

	/**
	 * コンテストの名前を返します。
	 *
	 *
	 * @return 名前
	 */
	@Override
	public final String toString() {
		return name();
	}

	/**
	 * コンテストの開催年を返します。
	 *
	 *
	 * @return 名前
	 *
	 * @since 2022/07/23
	 */
	public int year() {
		return Year.now().getValue();
	}

	/**
	 * コンテストの名前を返します。
	 *
	 *
	 * @return 名前
	 */
	public abstract String name();

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
	 * コンテストの開始日を計算します。
	 *
	 *
	 * @return 開始日
	 */
	public final LocalDate getStartDay() {
		return getStartDay(year());
	}

	/**
	 * コンテストの終了日を計算します。
	 *
	 *
	 * @return 終了日
	 */
	public final LocalDate getFinalDay() {
		return getFinalDay(year());
	}

	/**
	 * コンテストの締切日を計算します。
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
	 * 指定された部門をこの規約に追加します。
	 *
	 *
	 * @param section 追加する部門
	 *
	 * @return この規約
	 */
	public final Contest add(Section section) {
		this.map.put(section.name(), section);
		return this;
	}

	/**
	 * 指定された部門をこの規約から削除します。
	 *
	 *
	 * @param section 削除する部門
	 *
	 * @return この規約
	 */
	public final Contest remove(Section section) {
		this.map.remove(section.name());
		return this;
	}

	/**
	 * この規約の下の部門を反復子で返します。
	 *
	 *
	 * @return 全ての部門を含む反復子
	 */
	@Override
	public final Iterator<Section> iterator() {
		return map.values().iterator();
	}

	/**
	 * 指定された名前の部門を検索します。
	 *
	 *
	 * @param name 部門の名前
	 *
	 * @return 該当する部門
	 */
	public final Section section(String name) {
		return this.map.get(name);
	}

	/**
	 * この規約の部門の名前のリストを返します。
	 *
	 *
	 * @return 部門の名前のリスト
	 *
	 * @since 2022/07/18
	 */
	public final List<String> getSectionNames() {
		final var set = new LinkedHashSet<String>();
		for(final var s: this) set.add(s.name());
		final var seq = new LinkedList<String>(set);
		return Collections.unmodifiableList(seq);
	}

	/**
	 * この規約の部門の分類のリストを返します。
	 *
	 *
	 * @return 部門の分類のリスト
	 *
	 * @since 2022/07/18
	 */
	public final List<String> getSectionCodes() {
		final var set = new LinkedHashSet<String>();
		for(final var s: this) set.add(s.code());
		final var seq = new LinkedList<String>(set);
		return Collections.unmodifiableList(seq);
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
