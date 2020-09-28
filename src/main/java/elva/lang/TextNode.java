/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

/**
 * LISP処理系で使用される文字列のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/08
 */
public final class TextNode extends AtomBase<String> {
	private final String value;

	/**
	 * 指定された値で文字列を構築します。
	 *
	 *
	 * @param value 値
	 */
	public TextNode(String value) {
		this.value = value;
	}

	@Override
	public final String value() {
		return value;
	}

	@Override
	public final String toString() {
		return encode(this);
	}

	@Override
	public final int hashCode() {
		return value.hashCode();
	}

	@Override
	public final boolean equals(Object atom) {
		if(!TextNode.class.isInstance(atom)) return false;
		return ((TextNode) atom).value.equals(this.value);
	}

	/**
	 * 指定された文字列のエスケープ処理を行います。
	 *
	 *
	 * @param text 文字列
	 *
	 * @return 処理された文字列
	 */
	public static final String encode(TextNode text) {
		String code = text.value;
		code = code.replace("\\", "\\\\");
		code = code.replace("\"", "\\\"");
		code = code.replace("\t", "\\t");
		code = code.replace("\b", "\\b");
		code = code.replace("\n", "\\n");
		code = code.replace("\r", "\\r");
		code = code.replace("\f", "\\f");
		return "\"".concat(code).concat("\"");
	}

	/**
	 * 指定された文字列のエスケープ解除を行います。
	 *
	 *
	 * @param text 文字列
	 *
	 * @return 処理された文字列
	 */
	public static final TextNode decode(String text) {
		text = text.substring(1, text.length() - 1);
		text = text.replace("\\t", "\t");
		text = text.replace("\\b", "\b");
		text = text.replace("\\n", "\n");
		text = text.replace("\\r", "\r");
		text = text.replace("\\f", "\f");
		text = text.replace("\\\"", "\"");
		text = text.replace("\\\\", "\\");
		return new TextNode(text);
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class TEXT implements TypeRule {
		@Override
		public final boolean support(Object value) {
			return value instanceof String;
		}

		@Override
		public final NodeBase encode(Object value) {
			return new TextNode((String) value);
		}
	}
}
