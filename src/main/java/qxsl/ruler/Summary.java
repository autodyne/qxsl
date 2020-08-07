/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.*;
import java.util.stream.Collectors;

import static java.util.stream.IntStream.range;

/**
 * 有効な交信と無効な交信を保持するとともに識別子から得点を計算します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/26
 */
public final class Summary implements java.io.Serializable {
	private final List<Success> accepted;
	private final List<Success> distinct;
	private final List<Failure> rejected;
	private int total = 0;

	/**
	 * 有効な交信と無効な交信を指定してサマリを構築します。
	 *
	 * @param succ 受理された交信
	 * @param fail 拒否された交信
	 */
	public Summary(List<Success> succ, List<Failure> fail) {
		final var map = new LinkedHashMap<Object, Success>();
		for(Success it: succ) map.putIfAbsent(it.key(0), it);
		this.distinct = new ArrayList<>(map.values());
		this.accepted = new ArrayList<>(succ);
		this.rejected = new ArrayList<>(fail);
	}

	/**
	 * 指定された部門で有効な交信を列挙します。
	 *
	 * @return 有効な交信
	 */
	public final List<Success> accepted() {
		return Collections.unmodifiableList(accepted);
	}

	/**
	 * 指定された部門で無効な交信を列挙します。
	 *
	 * @return 無効な交信
	 */
	public final List<Failure> rejected() {
		return Collections.unmodifiableList(rejected);
	}

	/**
	 * 重複を排除した交信の得点の合計を返します。
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
	 * @return 総得点
	 */
	public final int total() {
		return this.total;
	}

	/**
	 * この交信記録の総得点を確定します。
	 *
	 *
	 * @param contest 規約
	 *
	 * @return この交信記録
	 */
	protected final Summary confirm(Contest contest) {
		this.total = contest.score(this);
		return this;
	}

	/**
	 * 指定された位置の識別子の重複を排除したリストを返します。
	 *
	 *
	 * @param nkey 識別子の位置
	 *
	 * @return 指定された位置の識別子の集合
	 *
	 * @since 2020/02/26
	 */
	public final List<Object> mults(int nkey) {
		var keys = distinct.stream().map(s -> s.key(nkey));
		return keys.distinct().collect(Collectors.toList());
	}

	/**
	 * 識別子の重複を排除したリストのリストを返します。
	 *
	 * @return 識別子の集合のリスト
	 *
	 * @since 2020/02/26
	 */
	public final List<List<Object>> mults() {
		var cnt = distinct.stream().mapToInt(Success::countKeys).min();
		final var sets = range(0, cnt.orElse(0)).mapToObj(this::mults);
		return sets.collect(Collectors.toList());
	}
}
