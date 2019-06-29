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
import qxsl.field.FieldMapper;
import qxsl.model.Field;
import qxsl.model.Item;

import static java.math.BigDecimal.ROUND_DOWN;

/**
 * 交信の波長帯を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/29
 *
 */
public final class Band extends Qxsl<BigDecimal> {
	private final BigDecimal band;

	/**
	 * 波長を指定して{@link Band}を構築します。
	 * 
	 * @param band メートル単位の波長
	 */
	public Band(double band) {
		this(BigDecimal.valueOf(band));
	}

	/**
	 * 波長を指定して{@link Band}を構築します。
	 * 
	 * @param band band単位の波長
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
	 * 波長帯を表す適切な単位の文字列を返します。
	 * 
	 * @return UI表示に適した文字列
	 */
	@Override
	public String toString() {
		if(band.doubleValue() < 1e-2) return toMMString();
		if(band.doubleValue() < 1e-0) return toCMString();
		return toMString();
	}

	/**
	 * 波長帯をメートル単位の文字列で返します。
	 * 
	 * @return メートル単位のUI表示に適した文字列
	 */
	public String toMString() {
		return toDecimalString(-0).concat("m");
	}

	/**
	 * 波長帯をセンチメートル単位の文字列で返します。
	 * 
	 * @return センチメートル単位のUI表示に適した文字列
	 */
	public String toCMString() {
		return toDecimalString(-2).concat("cm");
	}

	/**
	 * 波長帯をミリメートル単位の文字列で返します。
	 * 
	 * @return ミリメートル単位のUI表示に適した文字列
	 */
	public String toMMString() {
		return toDecimalString(-3).concat("mm");
	}

	/**
	 * この波長帯を実数で表現する文字列を返します。
	 * 
	 * @param scale 小数点の位置
	 * @return 実数により表される波長帯
	 */
	private String toDecimalString(int scale) {
		BigDecimal d = band.scaleByPowerOfTen(-scale);
		return d.stripTrailingZeros().toPlainString();
	}

	/**
	 * 単位付き文字列を解析してメートル単位の値を返します。
	 *
	 * @param text 単位付き文字列 "160m"等
	 * @return メートル単位の波長
	 */
	private static BigDecimal parse(String text) {
		final String FIX = "(?<=\\d)(?=[kcm]?m)";
		String[] tup = text.toLowerCase().split(FIX);
		final BigDecimal bd = new BigDecimal(tup[0]);
		switch(tup[1]) {
			case "km": return bd.scaleByPowerOfTen(+3);
			case "cm": return bd.scaleByPowerOfTen(-2);
			case "mm": return bd.scaleByPowerOfTen(-3);
		}
		return bd;
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 * 同じ波長帯の{@link Band}に対してtrueを返します。
	 * 
	 * @param obj 比較するオブジェクト
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
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2019/06/29
	 *
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

	/**
	 * {@link Band}への変換を行う変換器です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2019/06/29
	 *
	 */
	public static final class Mapper implements FieldMapper {
		@Override
		public QName target() {
			return BAND;
		}

		@Override
		public Band search(Item item) {
			Object call = item.value(new QName(ADIF, "BAND"));
			return call != null? new Band(call.toString()): null;
		}
	}
}
