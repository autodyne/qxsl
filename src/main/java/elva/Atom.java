/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Objects;

/**
 * LISP処理系で使用される空リストを除く不可分な値を格納します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2020/02/29
 */
public final class Atom implements Sexp, Serializable {
	private final Object value;

	/**
	 * 真を表す特殊なアトムです。
	 *
	 * @since 2020/02/29
	 */
	public static final Atom TRUE = new Atom(true);

	/**
	 * 指定された値でアトムを構築します。
	 *
	 * @param value 値
	 */
	protected Atom(Object value) {
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
	 * このアトムに格納された値を返します。
	 *
	 * @return nullの場合は例外を発生させる
	 * @throws ElvaRuntimeException 型検査の例外
	 */
	public final Object some() {
		return as(Object.class);
	}

	/**
	 * このアトムに格納された識別子を返します。
	 *
	 * @return 識別子
	 * @throws ElvaRuntimeException 型検査の例外
	 */
	public final Symbol name() {
		return as(Symbol.class);
	}

	/**
	 * このアトムに格納された文字列を返します。
	 *
	 * @return 文字列
	 * @throws ElvaRuntimeException 型検査の例外
	 */
	public final String text() {
		return as(String.class);
	}

	/**
	 * このアトムに格納された真偽値を返します。
	 *
	 * @return 真偽値
	 * @throws ElvaRuntimeException 型検査の例外
	 */
	public final boolean bool() {
		return as(Boolean.class);
	}

	/**
	 * このアトムに格納された実数値を返します。
	 * 任意の数値が実数に強制的に変換されます。
	 *
	 * @return 実数値
	 * @throws ElvaRuntimeException 型検査の例外
	 */
	public final BigDecimal real() {
		return BigDecimalRules.nToBD(as(Number.class));
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
		if (atom instanceof Atom) {
			final var v1 = ((Atom) atom).value;
			final var v2 = ((Atom) this).value;
			final var d1 = BigDecimalRules.apply(v1);
			final var d2 = BigDecimalRules.apply(v2);
			if(d1 == null) return Objects.equals(v1, v2);
			if(d2 == null) return Objects.equals(v1, v2);
			return d1.compareTo(d2) == 0;
		} else return false;
	}

	/**
	 * 数値型の実数への変換規則を実装します。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/29
	 */
	private static final class BigDecimalRules {
		/**
		 * 指定された値を実数に変換します。
		 *
		 * @param value 値
		 * @return 実数 数値でない場合はnull
		 */
		private static final BigDecimal apply(Object value) {
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
		private static final BigDecimal nToBD(Number value) {
			try {
				return (BigDecimal) value;
			} catch (ClassCastException ex) {
				// int32
				final var i2bd = iToBD(value);
				if (i2bd != null) return i2bd;
				// int64
				final var l2bd = lToBD(value);
				if (l2bd != null) return l2bd;
				return dToBD(value);
			}
		}

		/**
		 * 64ビット整数型の値を実数に変換します。
		 *
		 * @param value 整数型の値
		 * @return 実数
		 */
		private static final BigDecimal lToBD(Number value) {
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
		private static final BigDecimal iToBD(Number value) {
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
		private static final BigDecimal dToBD(Number value) {
			try {
				return BigDecimal.valueOf(value.doubleValue());
			} catch (ClassCastException ex) {
				return null;
			}
		}
	}
}
