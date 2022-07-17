/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

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
	 * コンテストの名前を返します。
	 *
	 *
	 * @return 名前
	 */
	public abstract String name();

	/**
	 * コンテストの主催者を返します。
	 *
	 *
	 * @return コンテストの主催者
	 */
	public abstract String host();

	/**
	 * コンテストの連絡先を返します。
	 *
	 *
	 * @return コンテストの連絡先
	 */
	public abstract String mail();

	/**
	 * コンテストの規約の場所を返します。
	 *
	 *
	 * @return コンテストの規約の場所
	 */
	public abstract String link();

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
	 * 指定された年の参加登録が受付可能か確認します。
	 *
	 *
	 * @param year 開催年
	 * @param zone タイムゾーン
	 *
	 * @return 現在時刻で受付可能な場合は真
	 */
	public boolean accept(int year, ZoneId zone) {
		return !expired(year, LocalDate.now(zone));
	}

	/**
	 * 指定された年の集計結果が閲覧可能か確認します。
	 *
	 *
	 * @param year 開催年
	 * @param zone タイムゾーン
	 *
	 * @return 現在時刻で閲覧可能な場合は真
	 */
	public boolean finish(int year, ZoneId zone) {
		return expired(year, LocalDate.now(zone));
	}

	/**
	 * 指定された年の締切日を経過した後か確認します。
	 *
	 *
	 * @param year 開催年
	 * @param date 時刻
	 *
	 * @return 経過後は真
	 */
	private boolean expired(int year, LocalDate date) {
		return date.isAfter(getDeadLine(year));
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
