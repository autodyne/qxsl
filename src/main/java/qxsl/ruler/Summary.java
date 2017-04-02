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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import qxsl.model.Item;

/**
 * 得点計算の結果を表現します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2016/11/26
 */
public final class Summary implements java.io.Serializable {
	private final Map<Object, Object> count = new HashMap<>();
	private final List<Item> accepted = new ArrayList<>();
	private final List<Item> rejected = new ArrayList<>();

	/**
	 * {@link Item}のリストと部門を指定してサマリを構築します。
	 *
	 * @param list 交信記録
	 * @param sect 参加部門
	 * @throws Exception LISPの評価で発生した何らかの例外
	 */
	public Summary(List<Item> list, Section sect) throws Exception {
		for(Item item: list) if(test(item, sect)) {
			Success succ = (Success) sect.validate(item);
			count.put(succ.call, succ.mult);
			accepted.add(item);
		} else {
			rejected.add(item);
		}
	}

	/**
	 * 指定された交信が有効かつ重複する交信がないか確認します。
	 *
	 * @param item 交信記録
	 * @param sect 部門
	 * @return 交信が得点に数えられる場合true
	 */
	private boolean test(Item item, Section sect) throws Exception {
		final Message message = sect.validate(item);
		if(message instanceof Failure) return false;
		return !count.containsKey(((Success) message).call);
	}

	/**
	 * 指定された部門で重複を排除した有効な交信回数を返します。
	 *
	 * @return 得点に数えられる交信回数
	 */
	public int calls() {
		return count.size();
	}

	/**
	 * 指定された部門で重複を排除した有効なマルチ数を返します。
	 *
	 * @return 得点に数えられるマルチ数
	 */
	public int mults() {
		return new HashSet<Object>(count.values()).size();
	}

	/**
	 * 交信回数にマルチ数を乗算して獲得した総得点を計算します。
	 *
	 * @return 獲得した総得点
	 */
	public int score() {
		return calls() * mults();
	}

	/**
	 * 指定された部門で有効かつ重複を排除した交信を列挙します。
	 *
	 * @return 有効な交信
	 */
	public List<Item> accepted() {
		return Collections.unmodifiableList(accepted);
	}

	/**
	 * 指定された部門で無効な交信を重複を排除せずに列挙します。
	 *
	 * @return 無効な交信
	 */
	public List<Item> rejected() {
		return Collections.unmodifiableList(rejected);
	}
}
