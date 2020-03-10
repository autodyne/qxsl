/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.util.Objects;

/**
 * LISP処理系で使用される真偽値のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/11
 */
public final class Bool extends Sexp implements Comparable<Bool> {
	private final boolean value;

	/**
	 * 真を表すアトムです。
	 */
	public static final Bool T = new Bool(true);

	/**
	 * 偽を表すアトムです。
	 */
	public static final Bool F = new Bool(false);

	/**
	 * 指定された値で真偽値を構築します。
	 *
	 * @param value 値
	 */
	private Bool(boolean value) {
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
	 * この真偽値のハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Boolean.hashCode(value);
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
		if(!Bool.class.isInstance(atom)) return false;
		else return ((Bool) atom).value == this.value;
	}

	/**
	 * この真偽値と指定された真偽値を比較します。
	 *
	 * @param text 右側の真偽値
	 * @return 比較した結果
	 */
	@Override
	public final int compareTo(Bool text) {
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
	public static final Bool asBool(Object sexp) {
		return (boolean) sexp? Bool.T: Bool.F;
	}

	/**
	 * 指定された値がこのアトム型に対応するか確認します。
	 *
	 * @param sexp 値
	 * @return 真偽値型の場合はtrue
	 */
	public static final boolean support(Object sexp) {
		return Boolean.class.isInstance(sexp);
	}
}
