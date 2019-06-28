/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.adif;

import java.math.BigDecimal;
import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.table.FieldFormat;

import static java.math.BigDecimal.ROUND_DOWN;

/**
 * 交信の波長帯を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/28
 *
 */
public final class Band extends Adif<BigDecimal> {
	private final BigDecimal band;
	private final Unit unit;

	/**
	 * メートル単位の波長を指定して{@link Band}を構築します。
	 * 
	 * @param band 波長
	 */
	public Band(BigDecimal band) {
		this(band, Unit.M);
	}

	/**
	 * 波長とそのスケールを指定して{@link Band}を構築します。
	 * 
	 * @param band 波長
	 * @param unit 波長の単位
	 */
	public Band(BigDecimal band, Unit unit) {
		super(BAND);
		this.band = band;
		this.unit = unit;
	}

	/**
	 * この波長をメートル単位に変換した値を返します。
	 *
	 * @return 波長 常にメートル単位
	 */
	@Override
	public BigDecimal value() {
		return band.scaleByPowerOfTen(-unit.scale);
	}

	/**
	 * この波長の初期化時に設定された単位を返します。
	 * 
	 * @return 単位の列挙子
	 */
	public Unit getUnit() {
		return unit;
	}

	/**
	 * 波長のSI接頭辞と単位の組を列挙します。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/06/28
	 */
	public enum Unit {
		 M (-0,  "m"),
		CM (-2, "cm"),
		MM (-3, "mm");
		private final int scale;
		private final String spell;
		private Unit(int scale, String spell) {
			this.scale = scale;
			this.spell = spell;
		}

		/**
		 * この列挙子の文字列を返します。
		 * 大文字と小文字は区別されます。
		 *
		 * @return 綴りの文字列
		 */
		@Override
		public String toString() {
			return this.spell;
		}
	}

	/**
	 * 波長帯を表す適切な単位の文字列を返します。
	 * 
	 * @return UI表示に適した文字列
	 */
	@Override
	public String toString() {
		return band.toPlainString().concat(unit.toString());
	}

	/**
	 * この波長で真空中の光速を除して周波数に変換します。
	 * 小数点以下の数値は負の無限大の方向に丸められます。
	 * 
	 * @return kHz単位の周波数
	 */
	public qxsl.extra.field.qxsl.Band convert() {
		BigDecimal lspd = BigDecimal.valueOf(299_792.458D);
		BigDecimal freq = lspd.divide(value(), ROUND_DOWN);
		return new qxsl.extra.field.qxsl.Band(freq);
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
		private static final String FIX = "(?<=\\d)(?=[CM]?M)";

		@Override
		public QName name() {
			return BAND;
		}

		@Override
		public Band decode(String value) {
			String[] vals = value.toUpperCase().split(FIX);
			final BigDecimal len = new BigDecimal(vals[0]);
			return new Band(len, Unit.valueOf(vals[1]));
		}

		@Override
		public String encode(Field field) {
			return field.toString();
		}
	}
}
