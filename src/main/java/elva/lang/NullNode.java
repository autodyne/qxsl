/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.lang;

/**
 * LISP処理系で使用される空値のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/07/25
 */
public final class NullNode extends AtomBase<Object> {
	private static final NullNode NULL = new NullNode();

	/**
	 * 秘匿されたコンストラクタです。
	 */
	private NullNode() {}

	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 *
	 * @return 値
	 */
	@Override
	public final Object value() {
		return null;
	}

	/**
	 * このアトムを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return "null";
	}

	/**
	 * このアトムからハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return 123527;
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
		return atom == this;
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/07/26
	 */
	public static final class NULL implements TypeRule {
		@Override
		public final boolean support(Object value) {
			return value == null;
		}

		@Override
		public final NodeBase encode(Object value) {
			return NullNode.NULL;
		}
	}
}
