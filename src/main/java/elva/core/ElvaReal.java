/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import elva.warn.ElvaRuntimeException;

import static java.math.MathContext.DECIMAL64;

/**
 * LISP処理系で使用される実数値のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/08
 */
public final class ElvaReal extends ElvaAtom implements Comparable<ElvaReal> {
	private static final Map<Class<?>, Class<?>> boxes = new HashMap<>();
	private final Number value;

	static {
		install(Long.class);
		install(Byte.class);
		install(Short.class);
		install(Float.class);
		install(Double.class);
		install(Integer.class);
	}

	/**
	 * 指定された値で実数値を構築します。
	 *
	 * @param value 値
	 */
	public ElvaReal(Number value) {
		this.value = value;
	}

	/**
	 * 指定された値で実数値を構築します。
	 *
	 * @param value 値
	 */
	public ElvaReal(String value) {
		this(new BigDecimal(value));
	}

	/**
	 * この実数値の値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final Number value() {
		return value;
	}

	/**
	 * この実数値の値を返します。
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
	 * この実数値とオブジェクトを比較します。
	 * 同じ内容の実数値であれば真を返します。
	 *
	 * @param atom 比較対象のオブジェクト
	 * @return 同じ内容の実数値のみtrue
	 */
	@Override
	public final boolean equals(Object atom) {
		if(ElvaReal.class.isInstance(atom)) {
			return this.compareTo((ElvaReal) atom) == 0;
		} else return false;
	}

	/**
	 * この実数値の文字列による表現を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		return String.valueOf(value);
	}

	/**
	 * 指定された実数値で加算します。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final ElvaReal add(ElvaReal real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new ElvaReal(l.add(r));
	}

	/**
	 * 指定された実数値で減算します。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final ElvaReal sub(ElvaReal real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new ElvaReal(l.subtract(r));
	}

	/**
	 * 指定された実数値で乗算します。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final ElvaReal mul(ElvaReal real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new ElvaReal(l.multiply(r));
	}

	/**
	 * 指定された実数値で除算します。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final ElvaReal div(ElvaReal real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new ElvaReal(l.divide(r, DECIMAL64));
	}

	/**
	 * 指定された実数値で剰余を求めます。
	 *
	 * @param real 右側の実数値
	 * @return 計算の結果
	 */
	public final ElvaReal mod(ElvaReal real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return new ElvaReal(l.remainder(r));
	}

	/**
	 * 指定された実数値との大小を比較します。
	 *
	 * @param real 右側の実数値
	 * @return 比較の結果
	 */
	@Override
	public final int compareTo(ElvaReal real) {
		final var l = this.toBigDecimal();
		final var r = real.toBigDecimal();
		return l.compareTo(r);
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

	/**
	 * 指定された数値型をラッパ型として登録します。
	 *
	 * @param type ラッパ型
	 */
	private static final void install(Class<?> type) {
		try {
			final var fld = type.getField("TYPE");
			final var cls = (Class) fld.get(null);
			boxes.put(cls, type);
		} catch (NoSuchFieldException ex) {
		} catch (IllegalAccessException ex) {}
	}

	/**
	 * この式が指定された型に適合するか確認します。
	 *
	 * @param type 型
	 * @return 式の値
	 */
	@Override
	public final Object isClass(Class<?> type) {
		final Class<?> box = boxes.get(type);
		return super.isClass(box == null? type: box);
	}

	/**
	 * 指定された値をこのアトム型で包みます。
	 *
	 * @param sexp 値
	 * @return 実数のアトム
	 *
	 * @throws ClassCastException 型検査の例外
	 */
	public static final ElvaReal asReal(Object sexp) {
		return new ElvaReal(Number.class.cast(sexp));
	}

	/**
	 * 指定された値がこのアトム型に対応するか確認します。
	 *
	 * @param sexp 値
	 * @return 数値型の場合は真
	 */
	public static final boolean support(Object sexp) {
		return Number.class.isInstance(sexp);
	}
}
