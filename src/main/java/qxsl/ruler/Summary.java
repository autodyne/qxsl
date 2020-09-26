/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

import qxsl.model.Item;

import static java.util.stream.IntStream.range;

/**
 * 有効な交信と無効な交信を保持するとともに識別子から得点を計算します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/26
 */
public final class Summary implements Serializable {
	private final List<Success> accepted;
	private final List<Success> distinct;
	private final List<Failure> rejected;
	private final int total;

	/**
	 * 指定された部門と交信記録を要約します。
	 *
	 *
	 * @param sect 部門
	 * @param list 交信記録
	 */
	public Summary(Section sect, List<Item> list) {
		this.accepted = new ArrayList<Success>();
		this.rejected = new ArrayList<Failure>();
		for(var item: list) {
			final var m = sect.verify(item);
			if(m instanceof Success) accepted.add((Success) m);
			if(m instanceof Failure) rejected.add((Failure) m);
		}
		final var map = new LinkedHashMap<Object, Success>();
		for(var it: accepted) map.putIfAbsent(it.key(0), it);
		this.distinct = new ArrayList<>(map.values());
		this.total = sect.score(this);
	}

	/**
	 * 指定された部門で有効な交信を列挙します。
	 *
	 *
	 * @return 有効な交信
	 */
	public final List<Success> accepted() {
		return Collections.unmodifiableList(accepted);
	}

	/**
	 * 指定された部門で無効な交信を列挙します。
	 *
	 *
	 * @return 無効な交信
	 */
	public final List<Failure> rejected() {
		return Collections.unmodifiableList(rejected);
	}

	/**
	 * 重複を排除した交信の得点の合計を返します。
	 *
	 *
	 * @return 得点に数えられる交信の得点の合計
	 *
	 * @since 2019/05/16
	 */
	public final int score() {
		return distinct.stream().mapToInt(Success::score).sum();
	}

	/**
	 * この交信記録の総得点を返します。
	 *
	 *
	 * @return 総得点
	 */
	public final int total() {
		return this.total;
	}

	/**
	 * 識別子を列挙した集合を返します。
	 *
	 *
	 * @param rank 識別子の位置
	 *
	 * @return 指定された位置の識別子の集合
	 *
	 * @since 2020/02/26
	 */
	private final Set<Object> keySet(int rank) {
		final var keys = distinct.stream().map(message -> message.key(rank));
		return Collections.unmodifiableSet(keys.collect(Collectors.toSet()));
	}

	/**
	 * 識別子を列挙した集合を先頭から並べたリストを返します。
	 *
	 *
	 * @return 識別子の集合のリスト
	 *
	 * @since 2020/02/26
	 */
	public final List<Set<Object>> keySets() {
		final var size = distinct.stream().mapToInt(Success::size).min();
		final var sets = range(0, size.orElse(0)).mapToObj(this::keySet);
		return sets.collect(Collectors.toList());
	}

	/**
	 * 得点と識別子を列挙した集合とを並べたリストを返します。
	 *
	 *
	 * @return 得点とそれに続く識別子集合のリスト
	 *
	 * @since 2020/09/03
	 */
	public final List<Object> toScoreAndKeys() {
		final var list = new ArrayList<Object>(Arrays.asList(score()));
		for(var keys: keySets()) list.add(new ArrayList<Object>(keys));
		return list;
	}
}
