/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.adif;

import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.table.FieldFormat;

/**
 * 交信の相手局の呼出符号を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/28
 *
 */
public final class Call extends Adif<String> {
	private final String callSign;

	/**
	 * コールサインを指定して{@link Call}を構築します。
	 * 
	 * @param callSign コールサイン
	 */
	public Call(String callSign) {
		super(CALL);
		this.callSign = callSign;
	}

	@Override
	public String value() {
		return callSign;
	}

	/**
	 * {@link Call}を生成するフォーマットです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/08
	 *
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName name() {
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
