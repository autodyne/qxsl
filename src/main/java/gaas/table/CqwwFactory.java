/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.Reader;
import java.io.Writer;

import qxsl.table.PrintFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

import gaas.field.Band;

/**
 * CQWWコンテストの交信記録を表すCabrillo書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/04
 */
public final class CqwwFactory extends PrintFactory {
	public static final String QSO = "QSO:";

	/**
	 * 書式を構築します。
	 */
	public CqwwFactory() {
		super("cqww", "ASCII");
	}

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param reader 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public final TableDecoder decoder(Reader reader) {
		return new CqwwDecoder(reader, this);
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param writer 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public final TableEncoder encoder(Writer writer) {
		return new CqwwEncoder(writer, this);
	}

	/**
	 * Cabrillo書式の周波数帯の列挙型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	public enum BandEnum {
		M1_8 ( "1800",       1_800),
		M3_5 ( "3500",       3_500),
		M7   ( "7000",       7_000),
		M14  ("14000",      14_000),
		M21  ("21000",      21_000),
		M28  ("28000",      28_000),
		M50  (   "50",      50_000),
		M70  (   "70",      70_000),
		M144 (  "144",     144_000),
		M222 (  "222",     222_000),
		M432 (  "432",     432_000),
		M902 (  "902",     902_000),
		G1_2 ( "1.2G",    1200_000),
		G2_3 ( "2.3G",    2300_000),
		G3_4 ( "3.4G",    3400_000),
		G5_7 ( "5.7G",    5700_000),
		G10  (  "10G",  10_000_000),
		G24  (  "24G",  24_000_000),
		G47  (  "47G",  47_000_000),
		G75  (  "75G",  75_000_000),
		G123 ( "123G", 123_000_000),
		G134 ( "134G", 134_000_000),
		G241 ( "241G", 241_000_000);

		private final Band band;
		private final String text;

		/**
		 * 指定された表記と周波数で列挙子を生成します。
		 *
		 *
		 * @param text 周波数の表記
		 * @param band 周波数
		 */
		private BandEnum(String text, int band) {
			this.text = text;
			this.band = new Band(band);
		}

		@Override
		public String toString() {
			return text;
		}

		/**
		 * この列挙子に対応するバンドを返します。
		 *
		 * @return バンド
		 */
		public Band toBand() {
			return band;
		}

		/**
		 * 指定された周波数に対応する列挙子を返します。
		 *
		 * @param band 周波数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static BandEnum valueOf(Band band) {
			for(var b: values()) if(b.band.equals(band)) return b;
			return null;
		}

		/**
		 * 指定された文字列に対応する列挙子を返します。
		 *
		 * @param text 文字列
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static BandEnum value(String text) {
			for(var b: values()) if(b.text.equals(text)) return b;
			return null;
		}
	}
}
