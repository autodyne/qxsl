/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

/**
 * LISP処理系で使用される汎用的なアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/29
 */
public final class AtomNode extends AtomBase<Object> {
	private final Object value;

	/**
	 * 指定された値でアトムを構築します。
	 *
	 *
	 * @param value 値
	 */
	public AtomNode(Object value) {
		this.value = value;
	}

	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 *
	 * @return 値
	 */
	@Override
	public final Object value() {
		return value;
	}

	/**
	 * このアトムを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return value.toString();
	}

	/**
	 * このアトムからハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return value.hashCode();
	}

	/**
	 * このアトムと指定された値を比較します。
	 *
	 *
	 * @param sexp 比較対象の値
	 *
	 * @return 等価の場合は真
	 */
	@Override
	public final boolean equals(Object atom) {
		if(!AtomNode.class.isInstance(atom)) return false;
		return ((AtomNode) atom).value.equals(this.value);
	}
}
