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
 * 交信の変調方式を表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Mode extends Qxsl<String> {
	private final String mode;

	/**
	 * モード名を指定して{@link Mode}を構築します。
	 *
	 * @param mode モード名
	 */
	public Mode(String mode) {
		super(MODE);
		this.mode = mode;
	}

	@Override
	public String value() {
		return mode;
	}

	/**
	 * {@link Mode}を生成する書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/09
	 */
	public static final class Factory implements FieldFactory {
		@Override
		public QName target() {
			return MODE;
		}

		@Override
		public Mode decode(String value) {
			return new Mode(value);
		}

		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
