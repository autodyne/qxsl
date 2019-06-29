/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field;

import java.math.BigDecimal;
import javax.xml.namespace.QName;
import qxsl.field.FieldFormat;
import qxsl.model.Field;

/**
 * 交信の周波数を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Freq extends Qxsl<BigDecimal> {
	private final BigDecimal kHz;

	/**
	 * 周波数を指定して{@link Freq}を構築します。
	 * 
	 * @param kHz kHz単位の周波数
	 */
	public Freq(int kHz) {
		this(BigDecimal.valueOf(kHz));
	}

	/**
	 * 周波数を指定して{@link Freq}を構築します。
	 * 
	 * @param kHz kHz単位の周波数
	 */
	public Freq(BigDecimal kHz) {
		super(FREQ);
		this.kHz = kHz;
	}

	@Override
	public BigDecimal value() {
		return kHz;
	}

	/**
	 * 周波数をキロヘルツ単位の整数値で返します。
	 *
	 * @return kHz単位の周波数
	 */
	public int toInt() {
		return value().intValue();
	}

	/**
	 * 周波数を表す適切な単位の文字列を返します。
	 * 
	 * @return UI表示に適した文字列
	 */
	@Override
	public String toString() {
		if(kHz.doubleValue() > 1e6) return toGHzString();
		if(kHz.doubleValue() > 1e3) return toMHzString();
		return toKHzString();
	}

	/**
	 * 周波数をキロヘルツ単位の文字列で返します。
	 * 
	 * @return kHz単位のUI表示に適した文字列
	 */
	public String toKHzString() {
		return toDecimalString(0).concat("kHz");
	}

	/**
	 * 周波数をメガヘルツ単位の文字列で返します。
	 * 
	 * @return MHz単位のUI表示に適した文字列
	 */
	public String toMHzString() {
		return toDecimalString(3).concat("MHz");
	}

	/**
	 * 周波数をギガヘルツ単位の文字列で返します。
	 * 
	 * @return GHz単位のUI表示に適した文字列
	 */
	public String toGHzString() {
		return toDecimalString(6).concat("GHz");
	}

	/**
	 * この周波数を実数で表現する文字列を返します。
	 * 
	 * @param scale 小数点の位置
	 * @return 実数により表される周波数
	 */
	private String toDecimalString(int scale) {
		return kHz.scaleByPowerOfTen(-scale).toPlainString();
	}

	/**
	 * {@link Freq}を生成する書式です。
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
			return FREQ;
		}

		@Override
		public Freq decode(String value) {
			return new Freq(new BigDecimal(value));
		}

		@Override
		public String encode(Field field) {
			return ((Freq) field).value().toPlainString();
		}
	}
}
