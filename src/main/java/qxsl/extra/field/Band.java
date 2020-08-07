/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import qxsl.field.FieldFormat;
import qxsl.model.Field;

import javax.xml.namespace.QName;
import java.math.BigDecimal;

/**
 * 交信の周波数を表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Band extends Qxsl<BigDecimal> {
	private final BigDecimal band;

	/**
	 * 周波数を指定して{@link Band}を構築します。
	 *
	 * @param band キロヘルツ単位の周波数
	 */
	public Band(int band) {
		this(BigDecimal.valueOf(band));
	}

	/**
	 * 周波数を指定して{@link Band}を構築します。
	 *
	 * @param band キロヘルツ単位の周波数
	 */
	public Band(BigDecimal band) {
		super(BAND);
		this.band = band;
	}

	/**
	 * 単位付きの文字列から{@link Band}を構築します。
	 *
	 * @param text 単位付きの文字列
	 */
	public Band(String text) {
		this(Band.parse(text));
	}

	@Override
	public BigDecimal value() {
		return band;
	}

	/**
	 * 周波数を表す適切な単位の文字列を返します。
	 *
	 * @return UI表示に適した文字列
	 */
	@Override
	public String toString() {
		if(band.doubleValue() > 1e6) return toGHzString();
		if(band.doubleValue() > 1e3) return toMHzString();
		return toKHzString();
	}

	/**
	 * 周波数をキロヘルツ単位の文字列で返します。
	 *
	 * @return キロヘルツ単位のUI表示に適した文字列
	 */
	public String toKHzString() {
		return toDecimalString(0).concat("kHz");
	}

	/**
	 * 周波数をメガヘルツ単位の文字列で返します。
	 *
	 * @return メガヘルツ単位のUI表示に適した文字列
	 */
	public String toMHzString() {
		return toDecimalString(3).concat("MHz");
	}

	/**
	 * 周波数をギガヘルツ単位の文字列で返します。
	 *
	 * @return ギガヘルツ単位のUI表示に適した文字列
	 */
	public String toGHzString() {
		return toDecimalString(6).concat("GHz");
	}

	/**
	 * この周波数を実数で表現する文字列を返します。
	 *
	 *
	 * @param scale 小数点の位置
	 *
	 * @return 実数により表される周波数
	 */
	private String toDecimalString(int scale) {
		BigDecimal d = band.scaleByPowerOfTen(-scale);
		return d.stripTrailingZeros().toPlainString();
	}

	/**
	 * 単位付き文字列を解析してキロヘルツ単位の値を返します。
	 *
	 *
	 * @param text 単位付き文字列 "1.9MHz"等
	 *
	 * @return キロヘルツ単位の波長
	 */
	private static BigDecimal parse(String text) {
		final String FIX = "(?<=\\d)(?=[kMG]?Hz)";
		String[] tup = text.split(FIX);
		final BigDecimal bd = new BigDecimal(tup[0]);
		switch(tup[1]) {
			case  "Hz": return bd.scaleByPowerOfTen(-3);
			case "kHz": return bd.scaleByPowerOfTen(+0);
			case "MHz": return bd.scaleByPowerOfTen(+3);
			case "GHz": return bd.scaleByPowerOfTen(+6);
		}
		throw new NumberFormatException(text);
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 * 同じ周波数の{@link Band}に対してtrueを返します。
	 *
	 *
	 * @param obj 比較するオブジェクト
	 *
	 * @return この属性と等しい場合true
	 */
	@Override
	public boolean equals(Object obj) {
		if(!(obj instanceof Band)) return false;
		return band.compareTo(((Band) obj).band) == 0;
	}

	/**
	 * {@link Band}を生成する書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/08
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName target() {
			return BAND;
		}

		@Override
		public Band decode(String value) {
			return new Band(new BigDecimal(value));
		}

		@Override
		public String encode(Field field) {
			return ((Band) field).value().toPlainString();
		}
	}
}
