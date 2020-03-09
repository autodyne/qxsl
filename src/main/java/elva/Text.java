/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.Serializable;
import java.util.Objects;

/**
 * LISP処理系で使用される文字列のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/08
 */
public final class Text extends Sexp implements Comparable<Text> {
	private final String value;

	/**
	 * 指定された値で文字列を構築します。
	 *
	 * @param value 値
	 */
	public Text(String value) {
		this.value = value;
	}

	/**
	 * この文字列の値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final Object value() {
		return value;
	}

	/**
	 * この文字列のハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Objects.hashCode(value);
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
		if(Text.class.isInstance(atom)) {
			final String val = ((Text) atom).value;
			return Objects.equals(val, this.value);
		} else return false;
	}

	/**
	 * この文字列と指定された文字列を比較します。
	 *
	 * @param text 右側の文字列
	 * @return 比較した結果
	 */
	@Override
	public final int compareTo(Text text) {
		return value.compareTo(text.value);
	}

	/**
	 * この文字列を式として表す文字列を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		return String.format("\"%s\"", value);
	}
}
