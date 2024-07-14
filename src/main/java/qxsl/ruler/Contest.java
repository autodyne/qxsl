/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.ruler;

import java.util.*;

import qxsl.utils.LevenDist;

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
	 * 指定された名前に似た部門を検索します。
	 *
	 *
	 * @param name 部門の名前
	 *
	 * @return 類似する部門
	 *
	 * @since 2024/07/14
	 */
	public final Section similar(String name) {
		final var shtein = LevenDist.comparator(name);
		final var stream = this.map.keySet().stream();
		return this.map.get(stream.min(shtein).get());
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
}
