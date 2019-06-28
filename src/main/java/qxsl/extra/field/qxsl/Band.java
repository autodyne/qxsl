/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.qxsl;

import java.math.BigDecimal;
import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.model.Tuple;
import qxsl.table.FieldFormat;

import static java.math.BigDecimal.ROUND_UP;

/**
 * 交信の周波数帯を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Band extends Qxsl<BigDecimal> {
	private final BigDecimal kHz;

	/**
	 * 周波数を指定して{@link Band}を構築します。
	 * 
	 * @param kHz kHz単位の周波数
	 */
	public Band(int kHz) {
		this(BigDecimal.valueOf(kHz));
	}

	/**
	 * 周波数を指定して{@link Band}を構築します。
	 * 
	 * @param kHz kHz単位の周波数
	 */
	public Band(BigDecimal kHz) {
		super(BAND);
		this.kHz = kHz;
	}

	@Override
	public BigDecimal value() {
		return kHz;
	}

	/**
	 * 周波数帯をキロヘルツ単位の整数値で返します。
	 *
	 * @return kHz単位の周波数
	 */
	public int toInt() {
		return value().intValue();
	}

	/**
	 * 周波数帯を表す適切な単位の文字列を返します。
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
	 * 周波数帯をキロヘルツ単位の文字列で返します。
	 * 
	 * @return kHz単位のUI表示に適した文字列
	 */
	public String toKHzString() {
		return toDecimalString(0) + "kHz";
	}

	/**
	 * 周波数帯をメガヘルツ単位の文字列で返します。
	 * 
	 * @return MHz単位のUI表示に適した文字列
	 */
	public String toMHzString() {
		return toDecimalString(3) + "MHz";
	}

	/**
	 * 周波数帯をギガヘルツ単位の文字列で返します。
	 * 
	 * @return GHz単位のUI表示に適した文字列
	 */
	public String toGHzString() {
		return toDecimalString(6) + "GHz";
	}

	/**
	 * この周波数帯を小数で表現する文字列を返します。
	 * 
	 * @param scale 小数点の位置
	 * @return 小数により表される周波数帯
	 */
	private String toDecimalString(int scale) {
		return kHz.scaleByPowerOfTen(-scale).toPlainString();
	}

	/**
	 * この周波数で真空中の光速を除して波長に変換します。
	 * 小数点以下の数値は正の無限大の方向に丸められます。
	 * 
	 * @return メートル単位の波長
	 */
	public qxsl.extra.field.adif.Band convert() {
		BigDecimal lspd = BigDecimal.valueOf(299792.458);
		BigDecimal band = lspd.divide(value(), ROUND_UP);
		return new qxsl.extra.field.adif.Band(band);
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
			return new Band(new BigDecimal(value));
		}

		@Override
		public String encode(Field field) {
			return ((Band) field).value().toPlainString();
		}
	}
}
