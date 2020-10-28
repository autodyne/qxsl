/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * コンテストの規約の実装はこの抽象クラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Contest extends Library implements Iterable<Section> {
	private final List<Section> list;

	/**
	 * 指定された部門を有する規約を構築します。
	 *
	 *
	 * @param sects 部門の集合
	 */
	public Contest(Section...sects) {
		this.list = new ArrayList<>();
		for(var s: sects) this.add(s);
	}

	/**
	 * コンテストの主催者を返します。
	 *
	 *
	 * @return コンテストの主催者
	 */
	public abstract String getHost();

	/**
	 * コンテストの連絡先を返します。
	 *
	 *
	 * @return コンテストの連絡先
	 */
	public abstract String getMail();

	/**
	 * コンテストの規約の場所を返します。
	 *
	 *
	 * @return コンテストの規約の場所
	 */
	public abstract String getLink();

	/**
	 * 指定された年のコンテストの開始日を計算します。
	 * 規約の実装に含まれる同名の関数を呼び出します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 開始日
	 */
	public final LocalDate getStartDay(int year) {
		return (LocalDate) invoke("getStartDay", year);
	}

	/**
	 * 指定された年のコンテストの終了日を計算します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 終了日
	 */
	public final LocalDate getFinalDay(int year) {
		return (LocalDate) invoke("getFinalDay", year);
	}

	/**
	 * 指定された年のコンテストの締切日を計算します。
	 *
	 *
	 * @param year 開催年
	 *
	 * @return 締切日
	 */
	public final LocalDate getDeadLine(int year) {
		return (LocalDate) invoke("getDeadLine", year);
	}

	/**
	 * 指定された部門をこの規約に追加します。
	 *
	 *
	 * @param sec 追加する部門
	 *
	 * @return この規約
	 */
	public final Contest add(Section sec) {
		this.list.add(sec);
		return this;
	}

	/**
	 * 指定された部門をこの規約から削除します。
	 *
	 *
	 * @param sec 削除する部門
	 *
	 * @return この規約
	 */
	public final Contest remove(Section sec) {
		this.list.remove(sec);
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
		return list.iterator();
	}

	/**
	 * この規約の下の部門をリストで返します。
	 *
	 *
	 * @return 全ての部門を含むリスト
	 */
	public final List<Section> getSections() {
		return Collections.unmodifiableList(list);
	}

	/**
	 * 指定された名前の部門を返し、未知の場合は例外を発生させます。
	 *
	 *
	 * @param name 部門の名前
	 *
	 * @return 該当する部門
	 *
	 * @throws NoSuchElementException 未知の部門の場合
	 */
	public final Section getSection(String name) {
		for(var s: this.list) if(s.getName().equals(name)) return s;
		throw new NoSuchElementException(name.concat(" not found"));
	}
}
