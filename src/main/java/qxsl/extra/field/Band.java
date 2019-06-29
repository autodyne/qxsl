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
	private final BigDecimal meter;

	/**
	 * 波長を指定して{@link Band}を構築します。
	 * 
	 * @param meter メートル単位の波長
	 */
	public Band(int meter) {
		this(BigDecimal.valueOf(meter));
	}

	/**
	 * 波長を指定して{@link Band}を構築します。
	 * 
	 * @param meter メートル単位の波長
	 */
	public Band(double meter) {
		this(BigDecimal.valueOf(meter));
	}

	/**
	 * 波長を指定して{@link Band}を構築します。
	 * 
	 * @param meter meter単位の波長
	 */
	public Band(BigDecimal meter) {
		super(BAND);
		this.meter = meter;
	}

	/**
	 * 単位付きの文字列から{@link Band}を構築します。
	 *
	 * @param text 単位(km,m,cm,mm)付きの文字列
	 */
	public Band(String text) {
		this(Band.parse(text));
	}

	@Override
	public BigDecimal value() {
		return meter;
	}

	/**
	 * 波長帯を表すメートル単位付きの文字列を返します。
	 * 
	 * @return UI表示に適した文字列
	 */
	@Override
	public String toString() {
		return meter.toPlainString().concat("m");
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
