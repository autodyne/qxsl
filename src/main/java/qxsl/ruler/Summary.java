/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import qxsl.model.Item;

import static java.util.stream.IntStream.range;

/**
 * 有効な交信と無効な交信を保持するとともに識別子から得点を計算します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2016/11/26
 */
public final class Summary implements java.io.Serializable {
	private final List<Success> accepted;
	private final List<Failure> rejected;

	/**
	 * 有効な交信と無効な交信を指定してサマリを構築します。
	 * この時点で先頭の識別子が重複する有効な交信は削除されます。
	 *
	 * @param succ 受理された交信
	 * @param fail 拒否された交信
	 */
	protected Summary(List<Success> succ, List<Failure> fail) {
		final HashMap<Object, Success> map = new LinkedHashMap<>();
		for(Success s: succ) map.putIfAbsent(s.key(0), s);
		succ = new ArrayList<>(map.values());
		this.accepted = Collections.unmodifiableList(succ);
		this.rejected = Collections.unmodifiableList(fail);
	}

	/**
	 * 交信の得点の合計に乗数を乗算して総得点を計算します。
	 *
	 * @return 総得点
	 * 
	 * @since 2019/05/16
	 */
	public int total() {
		return score() * mults();
	}

	/**
	 * 重複を排除した交信の得点の合計を返します。
	 *
	 * @return 得点に数えられる交信の得点の合計
	 * 
	 * @since 2019/05/16
	 */
	public int score() {
		return accepted.stream().mapToInt(Success::score).sum();
	}

	/**
	 * このサマリに含まれる全ての乗数の積を計算します。
	 * 先頭の識別子の異なり数は乗数には考慮されません。
	 *
	 * @return 乗数
	 */
	public int mults() {
		int cnt = accepted.stream().mapToInt(Success::countKeys).min().orElse(1);
		return range(1, cnt).map(this::count).reduce(1, Math::multiplyExact);
	}

	/**
	 * 指定された位置の識別子の異なり数を返します。
	 *
	 * @param keyNum 識別子の番号
	 * @return 指定された乗数の値
	 *
	 * @since 2019/05/16
	 */
	public int count(int keyNum) {
		return (int) accepted.stream().map(s -> s.key(keyNum)).distinct().count();
	}

	/**
	 * 指定された部門で有効かつ重複を排除した交信を列挙します。
	 *
	 * @return 有効な交信
	 */
	public List<Success> accepted() {
		return accepted;
	}

	/**
	 * 指定された部門で無効な交信を重複を排除せずに列挙します。
	 *
	 * @return 無効な交信
	 */
	public List<Failure> rejected() {
		return rejected;
	}
}
