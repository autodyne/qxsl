/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Objects;

import elva.Elva.ElvaRuntimeException;

/**
 * LISP処理系で使用される空リストを除く不可分な値を格納します。
 * 
 * 
 * @author 無線部開発班
 *
 * @since 2020/02/29
 */
public final class Atom extends Sexp implements Serializable {
	private static final RealRules rules = new RealRules();
	private final Object value;

	/**
	 * 指定された値でアトムを構築します。
	 *
	 * @param value 値
	 */
	public Atom(Object value) {
		this.value = value;
	}

	/**
	 * このアトムの値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final Object value() {
		return value;
	}

	/**
	 * このアトムに格納された実数値を返します。
	 * 任意の数値が実数に強制的に変換されます。
	 *
	 * @return 実数値
	 */
	public final BigDecimal real() {
		return rules.nToBD(as(Number.class));
	}

	/**
	 * このアトムのハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Objects.hashCode(value);
	}

	/**
	 * このアトムとオブジェクトを比較します。
	 * 同じ内容のアトムであれば真を返します。
	 *
	 * @param atom 比較対象のオブジェクト
	 * @return 同じ内容のアトムのみtrue
	 */
	@Override
	public final boolean equals(Object atom) {
		if(!Atom.class.isInstance(atom)) return false;
		final var val = ((Atom) atom).value;
		return this.equals(val, this.value);
	}

	/**
	 * 指定されたアトムの内容を比較します。
	 *
	 * @param v1 左側のアトムの内容
	 * @param v2 右側のアトムの内容
	 * @return 同じ内容の場合にtrue
	 */
	private final boolean equals(Object v1, Object v2) {
		if(v1 == null) return v2 == null;
		final BigDecimal d1 = rules.apply(v1);
		final BigDecimal d2 = rules.apply(v2);
		if(d1 == null) return v1.equals(v2);
		if(d2 == null) return v1.equals(v2);
		return d1.compareTo(d2) == 0;
	}

	/**
	 * このアトムの文字列による表現を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		if (!isString()) return Objects.toString(value);
		else return String.format("\"%s\"", this.value);
	}

	/**
	 * 数値型の実数への変換規則を実装します。
	 * 
	 * 
	 * @author 無線部開発班
	 *
	 * @since 2020/02/29
	 */
	private static final class RealRules {
		/**
		 * 指定された値を実数に変換します。
		 *
		 * @param value 値
		 * @return 実数 数値でない場合はnull
		 */
		private final BigDecimal apply(Object value) {
			try {
				return nToBD((Number) value);
			} catch (ClassCastException ex) {
				return null;
			}
		}

		/**
		 * 指定された数値を実数に変換します。
		 *
		 * @param value 数値
		 * @return 実数
		 */
		private final BigDecimal nToBD(Number value) {
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
}
