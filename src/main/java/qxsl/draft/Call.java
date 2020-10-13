/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import java.text.Normalizer;

import qxsl.value.Field;

import static java.text.Normalizer.Form.NFKC;

/**
 * 交信の相手局の呼出符号を表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Call extends Qxsl<String> {
	private static final String PATTERN = "\\w+(/\\w+)?";
	private final String call;

	/**
	 * 呼出符号を指定して属性を構築します。
	 *
	 *
	 * @param call 呼出符号
	 *
	 * @throws IllegalArgumentException 不正な文字を含む場合
	 */
	public Call(String call) {
		super(CALL);
		if(isValid(call)) this.call = normalize(call);
		else throw new IllegalArgumentException(call);
	}

	@Override
	public final String value() {
		return call;
	}

	/**
	 * 斜線より後の部分文字列を除く呼出符号を返します。
	 *
	 *
	 * @return 呼出符号
	 */
	public final String strip() {
		return call.split("/", 2)[0];
	}

	/**
	 * 指定された文字列が呼出符号であるかを確認します。
	 *
	 *
	 * @param call 文字列
	 *
	 * @return 正規形の呼出符号に変換可能な場合は真
	 *
	 * @since 2020/10/12
	 */
	public static final boolean isValid(String call) {
		return normalize(call).matches(PATTERN);
	}

	/**
	 * 指定された呼出符号を正規形の文字列に変換します。
	 *
	 *
	 * @param call 呼出符号
	 *
	 * @return 大文字または数字または記号の列
	 *
	 * @since 2020/10/12
	 */
	public static final String normalize(String call) {
		return Normalizer.normalize(call.toUpperCase(), NFKC);
	}
}
