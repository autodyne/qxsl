/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import javax.xml.namespace.QName;

import qxsl.field.FieldFactory;
import qxsl.model.Field;

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
	 * 呼出符号を指定して{@link Call}を構築します。
	 * 呼出符号が誤っている場合は例外が発生します。
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
	public String value() {
		return call;
	}

	/**
	 * 斜線より後の文字列を除く呼出符号を返します。
	 *
	 * @return 呼出符号
	 */
	public final String strip() {
		return call.split("/", 2)[0];
	}

	/**
	 * {@link Call}を生成する書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/08
	 */
	public static final class Factory implements FieldFactory {
		@Override
		public QName target() {
			return CALL;
		}

		@Override
		public Call decode(String value) {
			return new Call(value);
		}

		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
