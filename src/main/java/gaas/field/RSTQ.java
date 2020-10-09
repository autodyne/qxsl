/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.field;

import javax.xml.namespace.QName;

import qxsl.field.FieldFactory;
import qxsl.value.Field;

/**
 * 交信のRST(RSQ)レポートを表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class RSTQ extends Qxsl<Integer> {
	private final int r, s, t;

	/**
	 * 指定された整数で{@link RSTQ}を構築します。
	 * 3桁の整数の場合、音調レポートまで読み取ります。
	 * 2桁の整数の場合、音調レポートを読み取りません。
	 *
	 * @param rst RSTQをそのまま整数値にした値
	 */
	public RSTQ(int rst) {
		super(RSTQ);
		int r = (rst / 100) % 10;
		int s = (rst / 10 ) % 10;
		int t = (rst / 1  ) % 10;
		if(r > 0) {
			this.r = Math.max(1, Math.min(5, r));
			this.s = Math.max(1, Math.min(9, s));
			this.t = Math.max(1, Math.min(9, t));
		} else {
			this.r = Math.max(1, Math.min(5, s));
			this.s = Math.max(1, Math.min(9, t));
			this.t = 0;
		}
	}

	/**
	 * RSTQを整数で指定して{@link RSTQ}を構築します。
	 *
	 *
	 * @param r 了解度
	 * @param s 信号強度
	 * @param t 音調 または品質
	 */
	public RSTQ(int r, int s, int t) {
		this(r * 100 + s * 10 + t);
	}

	/**
	 * 了解度レポートを返します。
	 * この値は常に1以上5以下です。
	 *
	 * @return 了解度
	 */
	public int getR() {
		return r;
	}

	/**
	 * 信号強度レポートを返します。
	 * この値は常に1以上9以下です。
	 *
	 * @return 信号強度
	 */
	public int getS() {
		return s;
	}

	/**
	 * 音調レポートを返します。
	 * この値は常に0以上9以下です。
	 *
	 * @return 音調 もしくは品質
	 */
	public int getT() {
		return t;
	}

	@Override
	public Integer value() {
		if(this.t < 1) return this.r * 10 + this.s;
		return this.r * 100 + this.s * 10 + this.t;
	}

	/**
	 * {@link RSTQ}を生成する書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/08
	 */
	public static final class Factory implements FieldFactory {
		@Override
		public QName target() {
			return RSTQ;
		}

		@Override
		public RSTQ decode(String value) {
			return new RSTQ(Integer.parseInt(value));
		}

		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
