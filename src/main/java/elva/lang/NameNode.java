/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.lang;

import java.util.Arrays;

/**
 * LISP処理系で使用される識別子のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class NameNode extends AtomBase<NameNode> {
	private final String value;

	/**
	 * 名前を指定して識別子を生成します。
	 *
	 *
	 * @param value 名前
	 */
	public NameNode(String value) {
		this.value = value;
	}

	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 *
	 * @return 値
	 */
	@Override
	public final NameNode value() {
		return this;
	}

	/**
	 * このアトムを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return value;
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
	 * @param atom 比較対象の値
	 *
	 * @return 等価の場合は真
	 */
	@Override
	public final boolean equals(Object atom) {
		if(!NameNode.class.isInstance(atom)) return false;
		return ((NameNode) atom).value.equals(this.value);
	}

	/**
	 * この名前が可変長引数の名前であるか確認します。
	 *
	 *
	 * @return 可変長引数なら真
	 */
	public final boolean isVarArg() {
		return value.startsWith("*");
	}

	/**
	 * 構文解析時に参照される特殊な引用演算子を列挙します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/01
	 */
	public static enum Quote {
		QUOTE ("quote"),
		UQUOT ("unquote"),
		QUASI ("quasiquote"),
		UQSPL ("unquote-splicing");

		private final NameNode name;

		private Quote(String name) {
			this.name = new NameNode(name);
		}

		/**
		 * この演算子を使用して引用式を構築します。
		 *
		 *
		 * @param sexp 被引用式
		 *
		 * @return 引用式
		 */
		public final ListBase quote(NodeBase sexp) {
			return new ListNode(Arrays.asList(name, sexp));
		}

		/**
		 * 指定された式が引用式であるか確認します。
		 *
		 *
		 * @param sexp 式
		 *
		 * @return この演算子による引用の場合true
		 */
		public final boolean is(NodeBase sexp) {
			return ListBase.list(sexp).head().equals(name);
		}
	}
}
