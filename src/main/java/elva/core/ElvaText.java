/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.util.Objects;

/**
 * LISP処理系で使用される文字列のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/08
 */
public final class ElvaText extends ElvaAtom<String> {
	private final String value;

	/**
	 * 指定された値で文字列を構築します。
	 *
	 * @param value 値
	 */
	public ElvaText(String value) {
		this.value = value;
	}

	/**
	 * この文字列の値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final String value() {
		return value;
	}

	/**
	 * この文字列とオブジェクトを比較します。
	 * 同じ内容の文字列であれば真を返します。
	 *
	 * @param atom 比較対象のオブジェクト
	 * @return 同じ内容の文字列のみtrue
	 */
	@Override
	public final boolean equals(Object atom) {
		if(ElvaText.class.isInstance(atom)) {
			final String val = ((ElvaText) atom).value;
			return Objects.equals(val, this.value);
		} else return false;
	}

	/**
	 * この文字列を式として表す文字列を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		return encode(this);
	}

	/**
	 * 指定された文字列のエスケープ処理を行います。
	 *
	 * @param text 文字列
	 * @return 処理された文字列
	 */
	public static final String encode(ElvaText text) {
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
	 * @param text 文字列
	 * @return 処理された文字列
	 */
	public static final ElvaText decode(String text) {
		text = text.substring(1, text.length() - 1);
		text = text.replace("\\t", "\t");
		text = text.replace("\\b", "\b");
		text = text.replace("\\n", "\n");
		text = text.replace("\\r", "\r");
		text = text.replace("\\f", "\f");
		text = text.replace("\\\"", "\"");
		text = text.replace("\\\\", "\\");
		return new ElvaText(text);
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class Text implements Implicit {
		@Override
		public final boolean support(Object value) {
			return value instanceof String;
		}

		@Override
		public final ElvaNode encode(Object value) {
			return new ElvaText((String) value);
		}
	}
}
