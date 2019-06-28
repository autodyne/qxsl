/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.table.FieldFormat;

/**
 * 交信の空中線電力を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Watt extends Qxsl<String> {
	private final String watt;

	/**
	 * 空中線出力を指定して{@link Watt}を構築します。
	 * 
	 * @param watt 空中線出力
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
	 * {@link Watt}を生成するフォーマットです。
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
