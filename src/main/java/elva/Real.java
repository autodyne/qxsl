/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Objects;

import static java.math.MathContext.DECIMAL64;

/**
 * LISP処理系で使用される実数値のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/08
 */
public final class Real extends Sexp implements Comparable<Real> {
	private final Number value;

	/**
	 * 指定された値で実数値を構築します。
	 *
	 * @param value 値
	 */
	public Real(Number value) {
		this.value = value;
	}

	/**
	 * この実数値の値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final BigDecimal value() {
		try {
			return (BigDecimal) value;
		} catch (ClassCastException ex) {
			// int32
			final var bd32 = iToBD(value);
			if (bd32 != null) return bd32;
			// int64
			final var bd64 = lToBD(value);
			if (bd64 != null) return bd64;
			return dToBD(value);
		}
	}

	/**
	 * この実数値のハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Objects.hashCode(value);
	}

	/**
	 * この実数値とオブジェクトを比較します。
	 * 同じ内容の実数値であれば真を返します。
	 *
	 * @param atom 比較対象のオブジェクト
	 * @return 同じ内容の実数値のみtrue
	 */
	@Override
	public final boolean equals(Object atom) {
		if(Real.class.isInstance(atom)) {
			return this.compareTo((Real) atom) == 0;
		} else return false;
	}

	/**
	 * この実数値の文字列による表現を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		return Objects.toString(value);
	}

	/**
	 * 指定された実数値で加算します。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final Real add(Real real) {
		final var l = this.value();
		final var r = real.value();
		return new Real(l.add(r));
	}

	/**
	 * 指定された実数値で減算します。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final Real sub(Real real) {
		final var l = this.value();
		final var r = real.value();
		return new Real(l.subtract(r));
	}

	/**
	 * 指定された実数値で乗算します。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final Real mul(Real real) {
		final var l = this.value();
		final var r = real.value();
		return new Real(l.multiply(r));
	}

	/**
	 * 指定された実数値で除算します。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final Real div(Real real) {
		final var l = this.value();
		final var r = real.value();
		return new Real(l.divide(r, DECIMAL64));
	}

	/**
	 * 指定された実数値で剰余を求めます。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final Real mod(Real real) {
		final var l = this.value();
		final var r = real.value();
		return new Real(l.remainder(r));
	}

	/**
	 * 指定された実数値との大小を比較します。
	 *
	 * @param real 右側の実数値
	 * @return 比較の結果
	 */
	@Override
	public final int compareTo(Real real) {
		return value().compareTo(real.value());
	}

	/**
	 * 64ビット整数型の値を実数に変換します。
	 *
	 * @param value 整数型の値
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
	 * @param value 整数型の値
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
	 * @param value 浮動小数点型の値
	 * @return 実数
	 */
	private final BigDecimal dToBD(Number value) {
		return BigDecimal.valueOf(value.doubleValue());
	}
}
