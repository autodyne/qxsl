/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.math.BigDecimal;

import static java.math.MathContext.DECIMAL64;

/**
 * LISP処理系で使用される実数値のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/08
 */
public final class RealNode extends AtomBase<Number> {
	private final Number value;

	/**
	 * 指定された値で実数値を構築します。
	 *
	 * @param value 値
	 */
	public RealNode(Number value) {
		this.value = value;
	}

	/**
	 * 指定された値で実数値を構築します。
	 *
	 * @param value 値
	 */
	public RealNode(String value) {
		this(new BigDecimal(value));
	}

	@Override
	public final Number value() {
		return value;
	}

	@Override
	public final String toString() {
		return String.valueOf(value);
	}

	@Override
	public final int hashCode() {
		return value.hashCode();
	}

	@Override
	public final boolean equals(Object atom) {
		if(RealNode.class.isInstance(atom)) {
			return compareTo((RealNode) atom) == 0;
		} else return false;
	}

	/**
	 * 指定された実数値で加算します。
	 *
	 *
	 * @param real 右側の実数値
	 *
	 * @return 計算の結果
	 */
	public final RealNode add(RealNode real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new RealNode(l.add(r));
	}

	/**
	 * 指定された実数値で減算します。
	 *
	 *
	 * @param real 右側の実数値
	 *
	 * @return 計算の結果
	 */
	public final RealNode sub(RealNode real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new RealNode(l.subtract(r));
	}

	/**
	 * 指定された実数値で乗算します。
	 *
	 *
	 * @param real 右側の実数値
	 *
	 * @return 計算の結果
	 */
	public final RealNode mul(RealNode real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new RealNode(l.multiply(r));
	}

	/**
	 * 指定された実数値で除算します。
	 *
	 *
	 * @param real 右側の実数値
	 *
	 * @return 計算の結果
	 */
	public final RealNode div(RealNode real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new RealNode(l.divide(r, DECIMAL64));
	}

	/**
	 * 指定された実数値で剰余を求めます。
	 *
	 *
	 * @param real 右側の実数値
	 *
	 * @return 計算の結果
	 */
	public final RealNode mod(RealNode real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new RealNode(l.remainder(r));
	}

	/**
	 * 指定された実数値との大小を比較します。
	 *
	 *
	 * @param real 右側の実数値
	 *
	 * @return 比較の結果
	 */
	public final int compareTo(RealNode real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return l.compareTo(r);
	}

	/**
	 * 整数値を返します。
	 *
	 * @return 整数
	 */
	public final int toInt() {
		return toBigDecimal().intValueExact();
	}

	/**
	 * 実数値を返します。
	 *
	 * @return 値
	 */
	public final BigDecimal toBigDecimal() {
		try {
			return (BigDecimal) value;
		} catch (ClassCastException ex) {
			final var bd32 = iToBD(value);
			if (bd32 != null) return bd32;
			final var bd64 = lToBD(value);
			if (bd64 != null) return bd64;
			return dToBD(value);
		}
	}

	/**
	 * 64ビット整数型の値を実数に変換します。
	 *
	 *
	 * @param value 整数型の値
	 *
	 * @return 実数
	 */
	private final BigDecimal lToBD(Number value) {
		try {
			return BigDecimal.valueOf((long) value);
		} catch (ClassCastException ex) {
			return null;
		}
	}

	/**
	 * 32ビット整数型の値を実数に変換します。
	 *
	 *
	 * @param value 整数型の値
	 *
	 * @return 実数
	 */
	private final BigDecimal iToBD(Number value) {
		try {
			return BigDecimal.valueOf((int) value);
		} catch (ClassCastException ex) {
			return null;
		}
	}

	/**
	 * 64ビット浮動小数点型の値を実数に変換します。
	 *
	 *
	 * @param value 浮動小数点型の値
	 *
	 * @return 実数
	 */
	private final BigDecimal dToBD(Number value) {
		return BigDecimal.valueOf(value.doubleValue());
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class REAL implements TypeRule {
		@Override
		public final boolean support(Object value) {
			return value instanceof Number;
		}

		@Override
		public final NodeBase encode(Object value) {
			return new RealNode((Number) value);
		}
	}
}
