/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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

	@Override
	public final Object value() {
		return null;
	}

	@Override
	public final String toString() {
		return "null";
	}

	@Override
	public final int hashCode() {
		return 123527;
	}

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
