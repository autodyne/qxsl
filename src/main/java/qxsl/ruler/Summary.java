/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.ruler;

import java.io.Serializable;
import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import qxsl.model.Item;

import static java.util.stream.Collectors.toList;

/**
 * 有効な交信と無効な交信と得点を保持します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/26
 */
public final class Summary implements Serializable {
	private static final String DUPE = "DUPE";
	private final Map<Element, Message> acc;
	private final List<Message> rej;
	private final List<Element> mul;
	private final Section sec;

	/**
	 * 有効な交信と無効な交信を設定します。
	 *
	 *
	 * @param rule 判定基準
	 * @param list 交信記録
	 */
	public Summary(Section rule, List<Item> list) {
		this.sec = rule;
		this.acc = new LinkedHashMap<>();
		this.rej = new ArrayList<>(list.size());
		this.mul = new ArrayList<>(list.size());
		for(var item: list) addItem(item);
		for(var m: acc.values()) {
			final var item = m.item();
			mul.add(sec.entity(item));
		}
	}

	/**
	 * 有効な交信と無効な交信を分離します。
	 *
	 *
	 * @param item 交信記録
	 */
	private final void addItem(Item item) {
		final var msg = verifyItem(item);
		if(msg.isFailure()) rej.add(msg);
		else {
			final var idx = this.sec.unique(msg.item());
			if(!acc.containsKey(idx)) acc.put(idx, msg);
			else rej.add(new Failure(msg.item(), DUPE));
		}
	}

	/**
	 * 交信記録の検証結果または例外を返します。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @return 検証結果
	 *
	 * @since 2023/06/28
	 */
	private final Message verifyItem(Item item) {
		try {
			return sec.verify(item);
		} catch (RuntimeException ex) {
			return new Failure(item, ex.getMessage());
		}
	}

	/**
	 * 指定された部門で有効な交信を列挙します。
	 *
	 *
	 * @return 有効な交信
	 */
	public final List<Message> accepted() {
		return acc.values().stream().collect(toList());
	}

	/**
	 * 指定された部門で無効な交信を列挙します。
	 *
	 *
	 * @return 無効な交信
	 */
	public final List<Message> rejected() {
		return Collections.unmodifiableList(rej);
	}

	/**
	 * 交信で獲得した素点の合計値を計算します。
	 *
	 *
	 * @return 交信の得点の合計
	 *
	 * @since 2019/05/16
	 */
	public final int score() {
		return accepted().stream().mapToInt(Message::score).sum();
	}

	/**
	 * 素点とマルチ集合から総得点を計算します。
	 *
	 *
	 * @return 総得点
	 */
	public final int total() {
		return acc.isEmpty()? 0: sec.result(this);
	}

	/**
	 * 指定された位置のマルチ集合を集計します。
	 *
	 *
	 * @param rank 識別子の位置
	 *
	 * @return 指定された位置のマルチ集合
	 *
	 * @since 2020/02/26
	 */
	private final Set<Element> keys(int rank) {
		final var set = new HashSet<Element>();
		for(var el: mul) set.add(el.get(rank));
		return Collections.unmodifiableSet(set);
	}

	/**
	 * マルチ集合を順に並べたリストを返します。
	 *
	 *
	 * @return マルチ集合の列
	 *
	 * @since 2020/02/26
	 */
	public final List<Set<Element>> keySets() {
		final var size = this.mul.stream().mapToInt(Element::size);
		final var rank = IntStream.range(0, size.min().getAsInt());
		return rank.mapToObj(this::keys).collect(toList());
	}

	/**
	 * 得点とマルチ集合を並べた配列を返します。
	 *
	 *
	 * @return 得点とマルチ集合の配列
	 *
	 * @since 2020/09/03
	 */
	public final Object[] toArray() {
		final var score = Stream.of(this.score());
		final var mults = this.keySets().stream();
		final var lists = mults.map(Set::toArray);
		return Stream.concat(score, lists).toArray();
	}
}
