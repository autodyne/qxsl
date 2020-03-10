/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.util.Objects;

/**
 * LISP処理系で使用される汎用的なアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/29
 */
public final class Atom extends Sexp implements Comparable<Atom> {
	private final Object value;

	/**
	 * 指定された値でアトムを構築します。
	 *
	 * @param value 値
	 */
	public Atom(Object value) {
		this.value = value;
	}

	/**
	 * このアトムの値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final Object value() {
		return value;
	}

	/**
	 * このアトムのハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Objects.hashCode(value);
	}

	/**
	 * このアトムとオブジェクトを比較します。
	 * 同じ内容のアトムであれば真を返します。
	 *
	 * @param atom 比較対象のオブジェクト
	 * @return 同じ内容のアトムのみtrue
	 */
	@Override
	public final boolean equals(Object atom) {
		if(Atom.class.isInstance(atom)) {
			final Object val = ((Atom) atom).value;
			return Objects.equals(val, this.value);
		} else return false;
	}

	/**
	 * このアトムと指定されたアトムを比較します。
	 *
	 * @param atom 右側のアトム
	 * @return 比較した結果
	 */
	@Override
	public final int compareTo(Atom atom) {
		final String v1 = String.valueOf(this);
		final String v2 = String.valueOf(atom);
		return v1.compareTo(v2);
	}

	/**
	 * このアトムの文字列による表現を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		return String.valueOf(value);
	}
}
