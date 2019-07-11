/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.field;

import javax.xml.namespace.QName;
import qxsl.field.FieldFormat;
import qxsl.model.Field;

/**
 * 交信の相手局の呼出符号を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Call extends Qxsl<String> {
	private final String call;

	/**
	 * コールサインを指定して{@link Call}を構築します。
	 * 
	 * @param call コールサイン
	 */
	public Call(String call) {
		super(CALL);
		this.call = call;
	}

	@Override
	public String value() {
		return call;
	}

	/**
	 * {@link Call}を生成する書式です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/08
	 *
	 */
	public static final class Format implements FieldFormat {
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
