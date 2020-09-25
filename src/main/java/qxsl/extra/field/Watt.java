/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import javax.xml.namespace.QName;

import qxsl.field.FieldFactory;
import qxsl.model.Field;

/**
 * 交信の送信電力を表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Watt extends Qxsl<String> {
	private final String watt;

	/**
	 * 送信電力を指定して{@link Watt}を構築します。
	 *
	 * @param watt 送信電力
	 */
	public Watt(String watt) {
		super(WATT);
		this.watt = watt;
	}

	@Override
	public String value() {
		return watt;
	}

	/**
	 * {@link Watt}を生成する書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/08
	 */
	public static final class Factory implements FieldFactory {
		@Override
		public QName target() {
			return WATT;
		}

		@Override
		public Watt decode(String value) {
			return new Watt(value);
		}

		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
