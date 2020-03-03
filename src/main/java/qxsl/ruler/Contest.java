/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.Iterator;

/**
 * コンテストの規約の実装は{@link Contest}クラスを実装します。
 * 
 * 
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Contest implements Iterable<Section> {
	/**
	 * 秘匿されたコンストラクタです。
	 *
	 * @since 2020/02/28
	 */
	Contest() {}

	/**
	 * コンテスト名を返します。
	 *
	 * @return コンテスト名
	 */
	public abstract String getName();

	/**
	 * UIで表示するためにコンテスト名を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * このコンテストが提供する部門をイテレータで返します。
	 * 
	 * @return 全ての部門を含むイテレータ
	 */
	public abstract Iterator<Section> iterator();

	/**
	 * 指定された名前の{@link Section}を返します。
	 *
	 * @param name 部門の名前
	 * @return 該当する部門 見つからない場合はnull
	 */
	public final Section getSection(String name) {
		for(Section sec: this) {
			if(sec.getName().equals(name)) return sec;
		}
		return null;
	}

	/**
	 * 指定された交信記録の総得点を計算します。
	 *
	 * @param sum 交信記録
	 * @return 総得点
	 *
	 * @throws RuntimeException 得点計算で発生した何らかの例外
	 *
	 * @since 2020/02/26
	 */
	public abstract int score(Summary sum) throws RuntimeException;
}
