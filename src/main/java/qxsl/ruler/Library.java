/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

/**
 * 交信記録の処理のための手続きを提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/26
 */
public abstract class Library {
	/**
	 * コンテストの日程に型変換します。
	 *
	 *
	 * @return コンテストの日程
	 *
	 * @throws ClassCastException 型の誤り
	 */
	public final Program program() {
		return (Program) this;
	}

	/**
	 * コンテストの規約に型変換します。
	 *
	 *
	 * @return コンテストの規約
	 */
	public final Contest contest() {
		return (Contest) this;
	}

	/**
	 * コンテストの部門に型変換します。
	 *
	 *
	 * @return コンテストの部門
	 */
	public final Section section() {
		return (Section) this;
	}

	/**
	 * 交信記録の構造式に型変換します。
	 *
	 *
	 * @return 交信記録の構造
	 */
	public final Pattern pattern() {
		return (Pattern) this;
	}

	/**
	 * 規約が参照する変数値を返します。
	 *
	 *
	 * @param name 変数の名前
	 *
	 * @return 変数の値
	 *
	 * @since 2020/09/27
	 */
	public abstract Object get(String name);
}
