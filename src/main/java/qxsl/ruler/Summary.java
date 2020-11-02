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
	private final Map<Object, Message> acc;
	private final Map<Object, Message> rej;
	private final List<Object[]> mul;
	private final Formula fml;

	/**
	 * 有効な交信と無効な交信を設定します。
	 *
	 *
	 * @param form 判定基準
	 * @param list 交信記録
	 */
	public Summary(Formula form, List<Item> list) {
		this.mul = new ArrayList<>(list.size());
		this.acc = new LinkedHashMap<>();
		this.rej = new LinkedHashMap<>();
		this.fml = form;
		sort(list);
		accepted().map(fml::entity).forEach(mul::add);
	}

	/**
	 * 有効な交信と無効な交信を分離します。
	 *
	 *
	 * @param form 判定基準
	 * @param list 交信記録
	 */
	private final void sort(List<Item> list) {
		for(var item: list) {
			final var msg = fml.verify(item);
			final var idx = fml.unique(item);
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
		return acc.isEmpty()? 0: fml.result(this);
	}

	/**
	 * 指定された位置のマルチ集合を返します。
	 *
	 *
	 * @param rank 識別子の位置
	 *
	 * @return 指定された位置のマルチ集合
	 *
	 * @since 2020/02/26
	 */
	private final Set<Object> keys(int rank) {
		return mul.stream().map(m -> m[rank]).collect(toSet());
	}

	/**
	 * マルチ集合を順番のリストを返します。
	 *
	 *
	 * @return マルチ集合の列
	 *
	 * @since 2020/02/26
	 */
	public final Stream<Set<Object>> entity() {
		final var size = mul.stream().mapToInt(mul -> mul.length).min();
		return IntStream.range(0, size.getAsInt()).mapToObj(this::keys);
	}

	/**
	 * 得点とマルチ集合との配列を返します。
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
