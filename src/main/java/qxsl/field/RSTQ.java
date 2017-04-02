/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.model.FieldFormat;
import qxsl.table.secret.BaseFormat;

/**
 * 交信記録シートでRSTQレポートを表現します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class RSTQ extends Field<Integer> {
	private final int r, s, t;
	
	/**
	 * RSTQを整数で指定して{@link RSTQ}を構築します。
	 * 
	 * @param rst RSTQをそのまま整数値にした値
	 */
	public RSTQ(int rst) {
		super(BaseFormat.RSTQ);
		int r = (rst / 100) % 10;
		int s = (rst / 10 ) % 10;
		int t = (rst / 1  ) % 10;
		if(r > 0) {
			this.r = Math.max(0, Math.min(5, r));
			this.s = Math.max(0, Math.min(9, s));
			this.t = Math.max(0, Math.min(9, t));
		} else {
			this.r = Math.max(0, Math.min(9, s));
			this.s = Math.max(0, Math.min(9, t));
			this.t = 0;
		}
	}
	
	/**
	 * RSTQを整数で指定して{@link RSTQ}を構築します。
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
	 * 
	 * @return 了解度
	 */
	public int getR() {
		return r;
	}
	
	/**
	 * 信号強度レポートを返します。
	 * 
	 * @return 信号強度
	 */
	public int getS() {
		return s;
	}
	
	/**
	 * 音調レポートを返します。
	 * 
	 * @return 音調 もしくは品質
	 */
	public int getT() {
		return t;
	}

	@Override
	public Integer value() {
		if(t == 0)return r * 10 + s;
		return r * 100 + s * 10 + t;
	}

	/**
	 * {@link RSTQ}を生成するフォーマットです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/08
	 *
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName type() {
			return BaseFormat.RSTQ;
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
