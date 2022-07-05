/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import java.math.BigDecimal;

import gaas.draft.BandFactory;

/**
 * 交信の周波数帯を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Band extends Qxsl<BigDecimal> {
	/**
	 * 周波数を指定して属性を構築します。
	 *
	 *
	 * @param band キロヘルツ単位の周波数
	 */
	public Band(int band) {
		this(BigDecimal.valueOf(band));
	}

	/**
	 * 周波数を指定して属性を構築します。
	 *
	 *
	 * @param band キロヘルツ単位の周波数
	 */
	public Band(BigDecimal band) {
		super(BAND, band);
	}

	/**
	 * 指定された文字列が表す周波数を返します。
	 *
	 *
	 * @param text 単位付きの文字列
	 *
	 * @return 周波数帯
	 *
	 * @since 2022/07/04
	 */
	public static final Band parse(String text) {
		return (Band) new BandFactory().decode(text);
	}

	/**
	 * 周波数を表す適切な単位の文字列を返します。
	 *
	 *
	 * @return UI表示に適した文字列
	 */
	@Override
	public final String toString() {
		if(value().doubleValue() > 1e6) return toGHzString();
		if(value().doubleValue() > 1e3) return toMHzString();
		return toKHzString();
	}

	/**
	 * 周波数をキロヘルツ単位の文字列で返します。
	 *
	 *
	 * @return キロヘルツ単位のUI表示に適した文字列
	 */
	public final String toKHzString() {
		return toDecimalString(0).concat("kHz");
	}

	/**
	 * 周波数をメガヘルツ単位の文字列で返します。
	 *
	 *
	 * @return メガヘルツ単位のUI表示に適した文字列
	 */
	public final String toMHzString() {
		return toDecimalString(3).concat("MHz");
	}

	/**
	 * 周波数をギガヘルツ単位の文字列で返します。
	 *
	 *
	 * @return ギガヘルツ単位のUI表示に適した文字列
	 */
	public final String toGHzString() {
		return toDecimalString(6).concat("GHz");
	}

	/**
	 * この周波数を実数で表現する文字列を返します。
	 *
	 *
	 * @param exp 小数点の位置
	 *
	 * @return 実数により表される周波数
	 */
	public final String toDecimalString(int exp) {
		final var d = value().scaleByPowerOfTen(-exp);
		return d.stripTrailingZeros().toPlainString();
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 *
	 *
	 * @param obj 比較するオブジェクト
	 *
	 * @return この属性と等しい場合true
	 */
	@Override
	public final boolean equals(Object obj) {
		if(!Band.class.isInstance(obj)) return false;
		return value().compareTo(((Band) obj).value()) == 0;
	}
}
