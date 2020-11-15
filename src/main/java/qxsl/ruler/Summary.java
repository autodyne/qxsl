/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import qxsl.model.Item;

import static java.util.stream.Collectors.toSet;

/**
 * 有効な交信と無効な交信と得点を保持します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/26
 */
public final class Summary implements Serializable {
	private final Map<Element, Message> acc;
	private final Map<Element, Message> rej;
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
		this.mul = new ArrayList<>(list.size());
		this.acc = new LinkedHashMap<>();
		this.rej = new LinkedHashMap<>();
		this.sec = rule;
		sort(list);
		accepted().forEach(m -> mul.add(sec.entity(m.item())));
	}

	/**
	 * 有効な交信と無効な交信を分離します。
	 *
	 *
	 * @param list 交信記録
	 */
	private final void sort(List<Item> list) {
		for(var item: list) {
			final var msg = sec.verify(item);
			final var idx = sec.unique(msg.item());
			if(acc.containsKey(idx)) rej.put(idx, msg);
			else if(msg.isFailure()) rej.put(idx, msg);
			else if(msg.isSuccess()) acc.put(idx, msg);
		}
	}

	/**
	 * 指定された部門で有効な交信を列挙します。
	 *
	 *
	 * @return 有効な交信
	 */
	public final Stream<Message> accepted() {
		return acc.values().stream();
	}

	/**
	 * 指定された部門で無効な交信を列挙します。
	 *
	 *
	 * @return 無効な交信
	 */
	public final Stream<Message> rejected() {
		return rej.values().stream();
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
		return accepted().mapToInt(Message::score).sum();
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
		return mul.stream().map(m -> m.get(rank)).collect(toSet());
	}

	/**
	 * マルチ集合を順に並べたリストを返します。
	 *
	 *
	 * @return マルチ集合の列
	 *
	 * @since 2020/02/26
	 */
	public final Stream<Set<Element>> entity() {
		final var size = mul.stream().mapToInt(mul -> mul.size()).min();
		return IntStream.range(0, size.getAsInt()).mapToObj(this::keys);
	}

	/**
	 * 得点とマルチ集合を並べた配列を返します。
	 *
	 *
	 * @return 得点とマルチ集合の配列
	 *
	 * @since 2020/09/03
	 */
	public final Object[] toScoreAndEntitySets() {
		final var score = Stream.of(score());
		final var mults = entity().map(Set::toArray);
		return Stream.concat(score, mults).toArray();
	}
}
