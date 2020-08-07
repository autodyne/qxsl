/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * コンテストの規約の実装はこのクラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Contest implements Iterable<Section> {
	private final List<Section> list;

	/**
	 * 指定された部門を有する規約を構築します。
	 *
	 * @param sects 部門の集合
	 */
	protected Contest(Section...sects) {
		this.list = new ArrayList<>();
		for(var s: sects) this.add(s);
	}

	/**
	 * コンテストの名前を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * コンテストの名前を返します。
	 *
	 * @return コンテストの名前
	 */
	public abstract String getName();

	/**
	 * この規約の部門をイテレータで返します。
	 *
	 * @return 全ての部門を含むイテレータ
	 */
	public final Iterator<Section> iterator() {
		return this.list.iterator();
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
		this.list.add(section);
		section.setContest(this);
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
		this.list.remove(section);
		return this;
	}

	/**
	 * 指定された交信記録の総得点を計算します。
	 *
	 *
	 * @param summ 交信記録
	 *
	 * @return 総得点
	 *
	 * @since 2020/02/26
	 */
	public abstract int score(Summary summ);

	/**
	 * このコンテストに紐づけられた関数を実行します。
	 *
	 *
	 * @param name 関数の名前
	 * @param args 関数の引数
	 *
	 * @return 関数の値
	 *
	 * @since 2020/03/09
	 */
	public abstract Object invoke(String name, Object...args);

	/**
	 * 指定された名前の部門を返します。
	 * 未知の場合は例外を発生させます。
	 *
	 *
	 * @param name 部門の名前
	 *
	 * @return 該当する部門
	 *
	 * @throws NoSuchElementException 未知の部門の場合
	 */
	public final Section getSection(String name) {
		for(Section s: this) if(s.toString().equals(name)) return s;
		throw new NoSuchElementException(name.concat(" not found"));
	}
}
