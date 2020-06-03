/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.util.Arrays;
import java.util.Objects;

/**
 * LISP処理系で使用される識別子のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class ElvaName extends ElvaAtom implements Comparable<ElvaName> {
	private final String value;

	/**
	 * 名前を指定して識別子を生成します。
	 *
	 * @param value 名前
	 */
	public ElvaName(String value) {
		this.value = value;
	}

	/**
	 * この識別子の値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final ElvaName value() {
		return this;
	}

	/**
	 * この識別子の名前を返します。
	 *
	 * @return 名前
	 */
	@Override
	public final String toString() {
		return value;
	}

	/**
	 * この識別子とオブジェクトを比較します。
	 * 同じ名前の識別子であれば真を返します。
	 *
	 * @param atom 比較対象のオブジェクト
	 * @return 同じ名前の識別子のみtrue
	 */
	@Override
	public final boolean equals(Object atom) {
		if(!ElvaName.class.isInstance(atom)) return false;
		return ((ElvaName) atom).value.equals(this.value);
	}

	/**
	 * この識別子と指定された識別子を比較します。
	 *
	 * @param name 右側の識別子
	 * @return 比較した結果
	 */
	@Override
	public final int compareTo(ElvaName name) {
		return value.compareTo(name.value);
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

		private final ElvaName name;
		private Quote(String name) {
			this.name = new ElvaName(name);
		}

		/**
		 * この演算子を使用して引用式を構築します。
		 *
		 * @param sexp 被引用式
		 * @return 引用式
		 */
		public final ElvaList quote(ElvaNode sexp) {
			return ElvaList.chain(Arrays.asList(name, sexp));
		}

		/**
		 * 指定された式が引用式であるか確認します。
		 *
		 * @param sexp 式
		 * @return この演算子による引用の場合true
		 */
		public final boolean is(ElvaNode sexp) {
			return ElvaList.cast(sexp).head().equals(name);
		}
	}

	/**
	 * この名前の演算子を引数に適用する式を返します。
	 *
	 * @param args 被演算子
	 * @return 演算子及び被演算子のリスト
	 */
	public final ElvaList chain(Object...args) {
		return new ElvaList.Chain(this, new ElvaList.Array(args));
	}
}
