/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import qxsl.value.Field;

import static java.text.Normalizer.Form.NFKC;
import static java.text.Normalizer.normalize;

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
		call = normalize(call.toUpperCase(), NFKC);
		if(call.matches(PATTERN)) this.call = call;
		else throw new IllegalArgumentException(call);
	}

	@Override
	public final String value() {
		return call;
	}

	/**
	 * 斜線より後の文字列を除く呼出符号を返します。
	 *
	 *
	 * @return 呼出符号
	 */
	public final String strip() {
		return call.split("/", 2)[0];
	}
}
