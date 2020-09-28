/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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

	@Override
	public final NameNode value() {
		return this;
	}

	@Override
	public final String toString() {
		return value;
	}

	@Override
	public final int hashCode() {
		return value.hashCode();
	}

	@Override
	public final boolean equals(Object atom) {
		if(!NameNode.class.isInstance(atom)) return false;
		return ((NameNode) atom).value.equals(this.value);
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
			return new CoverSeq(Arrays.asList(name, sexp));
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
