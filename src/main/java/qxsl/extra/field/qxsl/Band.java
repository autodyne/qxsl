/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.qxsl;

import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.table.FieldFormat;

/**
 * 交信の周波数帯を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Band extends Qxsl<Integer> {
	private final int kHz;
	
	/**
	 * 周波数を指定して{@link Band}を構築します。
	 * 
	 * @param kHz kHz単位の周波数
	 */
	public Band(int kHz) {
		super(BAND);
		this.kHz = kHz;
	}

	@Override
	public Integer value() {
		return kHz;
	}
	
	/**
	 * 周波数帯を表す適切な単位の文字列を返します。
	 * 
	 * @return UI表示に適した文字列
	 */
	@Override
	public String toString() {
		int MHz = kHz / 1000;
		int GHz = MHz / 1000;
		if(GHz > 0) return toGHzString();
		if(MHz > 0) return toMHzString();
		return toKHzString();
	}
	
	/**
	 * 周波数帯をキロヘルツ単位の文字列で返します。
	 * 
	 * @return kHz単位のUI表示に適した文字列
	 */
	public String toKHzString() {
		return toDecimalString(1) + "kHz";
	}
	
	/**
	 * 周波数帯をメガヘルツ単位の文字列で返します。
	 * 
	 * @return MHz単位のUI表示に適した文字列
	 */
	public String toMHzString() {
		return toDecimalString(1_000) + "MHz";
	}
	
	/**
	 * 周波数帯をギガヘルツ単位の文字列で返します。
	 * 
	 * @return GHz単位のUI表示に適した文字列
	 */
	public String toGHzString() {
		return toDecimalString(1_000_000) + "GHz";
	}
	
	/**
	 * この周波数帯を小数で表現する文字列を返します。
	 * 
	 * @param decimalPoint 小数点の桁(kHz単位)
	 * @return 小数により表される周波数帯
	 */
	private String toDecimalString(int decimalPoint) {
		StringBuilder decimal = new StringBuilder();
		boolean ignored = true;
		for(int c = 1; c < decimalPoint; c *= 10) {
			int num = kHz / c % 10;
			if(num > 0 && ignored) ignored = false;
			if(!ignored) decimal.append(num);
		}
		StringBuilder sb = new StringBuilder();
		sb.append(kHz / decimalPoint);
		if(decimal.length() > 0) sb.append('.');
		return sb.append(decimal.reverse()).toString();
	}
	
	/**
	 * {@link Band}を生成するフォーマットです。
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
			return BAND;
		}
	
		@Override
		public Band decode(String value) {
			return new Band(Integer.parseInt(value));
		}
	
		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
