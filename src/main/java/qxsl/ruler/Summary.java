/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
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
public final class Summary implements java.io.Serializable {
	private final List<Success> accepted;
	private final List<Failure> rejected;
	private int total = 0;

	/**
	 * 有効な交信と無効な交信を指定してサマリを構築します。
	 * この時点で先頭の識別子が重複する有効な交信は削除されます。
	 *
	 * @param succ 受理された交信
	 * @param fail 拒否された交信
	 */
	protected Summary(List<Success> succ, List<Failure> fail) {
		final var map = new LinkedHashMap<Object, Success>();
		for(Success s: succ) map.putIfAbsent(s.key(0), s);
		final var acc = new ArrayList<>(map.values());
		this.accepted = Collections.unmodifiableList(acc);
		this.rejected = Collections.unmodifiableList(fail);
	}

	/**
	 * 指定された部門で有効かつ重複を排除した交信を列挙します。
	 *
	 * @return 有効な交信
	 */
	public final List<Success> accepted() {
		return accepted;
	}

	/**
	 * 指定された部門で無効な交信を重複を排除せずに列挙します。
	 *
	 * @return 無効な交信
	 */
	public final List<Failure> rejected() {
		return rejected;
	}

	/**
	 * 重複を排除した交信の得点の合計を返します。
	 *
	 * @return 得点に数えられる交信の得点の合計
	 *
	 * @since 2019/05/16
	 */
	public final int score() {
		return accepted.stream().mapToInt(Success::score).sum();
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
	 * @param contest 規約
	 * @return この交信記録
	 */
	protected final Summary confirm(Contest contest) {
		this.total = contest.score(this);
		return this;
	}

	/**
	 * 指定された位置の識別子の重複を排除した集合を返します。
	 *
	 * @param nkey 識別子の位置
	 * @return 指定された位置の識別子の集合
	 *
	 * @since 2020/02/26
	 */
	public final Set<Object> mults(int nkey) {
		var keys = accepted.stream().map(s -> s.key(nkey));
		return keys.distinct().collect(Collectors.toSet());
	}

	/**
	 * 識別子の重複を排除した集合のリストを返します。
	 *
	 * @return 識別子の集合のリスト
	 *
	 * @since 2020/02/26
	 */
	public final List<Set<Object>> mults() {
		var cnt = accepted.stream().mapToInt(Success::countKeys).min();
		final var sets = range(0, cnt.orElse(0)).mapToObj(this::mults);
		return sets.collect(Collectors.toList());
	}
}
