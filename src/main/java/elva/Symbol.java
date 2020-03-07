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
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class Symbol implements Serializable {
	private final String name;

	/**
	 * 名前を指定して識別子を生成します。
	 *
	 * @param name 名前
	 */
	public Symbol(String name) {
		this.name = name;
	}

	/**
	 * この識別子の名前を返します。
	 *
	 * @return 名前
	 */
	@Override
	public final String toString() {
		return name;
	}

	/**
	 * この識別子のハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return name.hashCode();
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
		if(!Symbol.class.isInstance(obj)) return false;
		return ((Symbol) obj).name.equals(this.name);
	}

	/**
	 * 構文解析時に参照される特殊な引用演算子を列挙します。
	 *
	 *
	 * @author 無線部開発班
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
		 * この演算子を使用して引用式を構築します。
		 *
		 * @param sexp 被引用式
		 * @return 引用式
		 */
		public final Cons quote(Sexp sexp) {
			return Cons.cons(new Atom(name), sexp);
		}

		/**
		 * 指定された式が引用式であるか確認します。
		 *
		 * @param sexp 式
		 * @return この演算子による引用の場合true
		 */
		public final boolean is(Sexp sexp) {
			return Cons.cast(sexp).car().value().equals(name);
		}
	}
}
