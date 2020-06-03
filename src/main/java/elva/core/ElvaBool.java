/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.util.Objects;

/**
 * LISP処理系で使用される真偽値のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/11
 */
public final class ElvaBool extends ElvaAtom implements Comparable<ElvaBool> {
	private final boolean value;

	/**
	 * 真を表すアトムです。
	 */
	public static final ElvaBool T = new ElvaBool(true);

	/**
	 * 偽を表すアトムです。
	 */
	public static final ElvaBool F = new ElvaBool(false);

	/**
	 * 指定された値で真偽値を構築します。
	 *
	 * @param value 値
	 */
	private ElvaBool(boolean value) {
		this.value = value;
	}

	/**
	 * この真偽値の値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final Boolean value() {
		return value;
	}

	/**
	 * この真偽値とオブジェクトを比較します。
	 * 同じ内容の真偽値であれば真を返します。
	 *
	 * @param atom 比較対象のオブジェクト
	 * @return 同じ内容の真偽値のみtrue
	 */
	@Override
	public final boolean equals(Object atom) {
		if(!ElvaBool.class.isInstance(atom)) return false;
		else return ((ElvaBool) atom).value == this.value;
	}

	/**
	 * この真偽値と指定された真偽値を比較します。
	 *
	 * @param text 右側の真偽値
	 * @return 比較した結果
	 */
	@Override
	public final int compareTo(ElvaBool text) {
		return value().compareTo(text.value);
	}

	/**
	 * この真偽値を式として表す真偽値を返します。
	 *
	 * @return 真偽値
	 */
	@Override
	public final String toString() {
		return value? "#t": "#f";
	}

	/**
	 * 指定された値をこのアトム型で包みます。
	 *
	 * @param sexp 値
	 * @return 真偽値のアトム
	 *
	 * @throws ClassCastException 型検査の例外
	 */
	public static final ElvaBool asBool(Object sexp) {
		return (boolean) sexp? ElvaBool.T: ElvaBool.F;
	}

	/**
	 * 指定された値がこのアトム型に対応するか確認します。
	 *
	 * @param sexp 値
	 * @return 真偽値型の場合は真
	 */
	public static final boolean support(Object sexp) {
		return Boolean.class.isInstance(sexp);
	}
}
