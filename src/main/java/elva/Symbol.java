/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.Serializable;

/**
 * LISP処理系で使用される識別子の実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/18
 */
public final class Symbol implements Serializable {
	private final String id;

	/**
	 * 名前を指定して識別子を生成します。
	 *
	 * @param id 名前
	 */
	public Symbol(String id) {
		this.id = id;
	}

	/**
	 * この識別子の名前を返します。
	 *
	 * @return 名前
	 */
	@Override
	public final String toString() {
		return id;
	}

	/**
	 * この識別子のハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return id.hashCode();
	}

	/**
	 * この識別子とオブジェクトを比較します。
	 * 同じ名前の識別子であれば真を返します。
	 *
	 * @param obj 比較対象のオブジェクト
	 * @return 同じ名前の識別子のみtrue
	 */
	@Override
	public final boolean equals(Object obj) {
		if (!(obj instanceof Symbol)) return false;
		return obj.toString().equals(toString());
	}

	/**
	 * 構文解析時に参照される特殊な引用演算子を列挙します。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/07/01
	 */
	enum Quote {
		QUOTE ("quote"),
		UQUOT ("unquote"),
		QUASI ("quasiquote"),
		UQSPL ("unquote-splicing");

		private final Symbol name;
		private Quote(String name) {
			this.name = new Symbol(name);
		}

		/**
		 * この演算子の名前をアトムとして返します。
		 *
		 * @return 演算子の識別子を包むアトム
		 */
		public final Atom toAtom() {
			return new Atom(name);
		}

		/**
		 * この演算子を使用して引用式を構築します。
		 *
		 * @param sexp 被引用式
		 * @return 引用式
		 */
		public final Cons quote(Sexp sexp) {
			return Cons.cons(toAtom(), sexp);
		}
	}
}
