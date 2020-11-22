/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

/**
 * LISP処理系で使用される真偽値のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/11
 */
public final class BoolNode extends AtomBase<Boolean> {
	private final boolean value;

	/**
	 * 真を表すアトムです。
	 */
	public static final BoolNode T = new BoolNode(true);

	/**
	 * 偽を表すアトムです。
	 */
	public static final BoolNode F = new BoolNode(false);

	/**
	 * 指定された値で真偽値を構築します。
	 *
	 * @param value 値
	 */
	private BoolNode(boolean value) {
		this.value = value;
	}

	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 *
	 * @return 値
	 */
	@Override
	public final Boolean value() {
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
		return value? "#t": "#f";
	}

	@Override
	public final int hashCode() {
		return Boolean.hashCode(value);
	}

	@Override
	public final boolean equals(Object atom) {
		if(!BoolNode.class.isInstance(atom)) return false;
		else return ((BoolNode) atom).value == this.value;
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class BOOL implements TypeRule {
		@Override
		public final boolean support(Object value) {
			return value instanceof Boolean;
		}

		@Override
		public final NodeBase encode(Object value) {
			return (boolean) value? BoolNode.T: BoolNode.F;
		}
	}
}
