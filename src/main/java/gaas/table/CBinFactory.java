/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.InputStream;
import java.io.OutputStream;
import java.time.Instant;
import java.time.ZonedDateTime;

import qxsl.draft.Band;
import qxsl.draft.Mode;
import qxsl.draft.Time;
import qxsl.table.BasicFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

import static java.time.ZoneOffset.UTC;
import static java.time.temporal.ChronoUnit.SECONDS;

/**
 * CTESTWINバイナリファイルのうちLG8と呼ばれる書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/06/12
 */
public final class CBinFactory extends BasicFactory {
	/**
	 * 書式を構築します。
	 */
	public CBinFactory() {
		super("cbin");
	}

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param is 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public final TableDecoder decoder(InputStream is) {
		return new CBinDecoder(is, this);
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param os 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public final TableEncoder encoder(OutputStream os) {
		return new CBinEncoder(os, this);
	}

	/**
	 * LG8書式の周波数帯の列挙型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/06/12
	 */
	public enum BandEnum {
		M1_9 (     1900),
		M3_5 (     3500),
		M7   (     7000),
		M10  (    10000),
		M14  (    14000),
		M18  (    18000),
		M21  (    21000),
		M24  (    24000),
		M28  (    28000),
		M50  (    50000),
		M144 (   144000),
		M430 (   430000),
		M1200(  1200000),
		M2400(  2400000),
		M5600(  5600000),
		G10  ( 10000000),
		G24  ( 24000000),
		G47  ( 47000000),
		G75  ( 75000000),
		G77  ( 77000000),
		G135 (135000000),
		G248 (248000000),
		K136 (      136);

		private final Band band;

		/**
		 * 周波数帯を指定して列挙子を生成します。
		 *
		 *
		 * @param band 周波数帯
		 */
		private BandEnum(int kHz) {
			this.band = new Band(kHz);
		}

		@Override
		public String toString() {
			return band.toString();
		}

		/**
		 * この列挙子に対応する周波数帯を返します。
		 *
		 * @return 周波数帯
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
			for(var v: values()) if(v.band.equals(band)) return v;
			return null;
		}

		/**
		 * 指定された序数に対応する列挙子を返します。
		 *
		 * @param band 序数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static BandEnum forIndex(int band) {
			for(var v: values()) if(v.ordinal() == band) return v;
			return null;
		}
	}

	/**
	 * LG8書式の通信方式の列挙型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/06/12
	 */
	public enum ModeEnum {
		CW     ("CW"),
		RTTY   ("RTTY"),
		SSB    ("SSB"),
		FM     ("FM"),
		AM     ("AM"),
		ATV    ("ATV"),
		SSTV   ("SSTV"),
		PSK    ("PSK"),
		GMSK   ("GMSK"),
		MFSK   ("MFSK"),
		QPSK   ("QPSK"),
		FSK    ("FSK"),
		DSTAR  ("D-STAR"),
		C4FM   ("C4FM"),
		JT65   ("JT65"),
		JT9    ("JT9"),
		ISCAT  ("ISCAT"),
		FT8    ("FT8"),
		JT4    ("JT4"),
		QRA64  ("QRA64"),
		MSK144 ("MSK144"),
		WSPR   ("WSPR"),
		JTMS   ("JTMS"),
		FT4    ("FT4");

		private final Mode mode;

		/**
		 * 通信方式を指定して列挙子を生成します。
		 *
		 *
		 * @param mode 通信方式
		 */
		private ModeEnum(String mode) {
			this.mode = new Mode(mode);
		}

		/**
		 * この列挙子に対応するモードを返します。
		 *
		 * @return モード
		 */
		public Mode toMode() {
			return mode;
		}

		/**
		 * 指定されたモードに対応する列挙子を返します。
		 *
		 * @param mode モード
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static ModeEnum valueOf(Mode mode) {
			for(var v: values()) if(v.mode.equals(mode)) return v;
			return null;
		}

		/**
		 * 指定された序数に対応する列挙子を返します。
		 *
		 * @param mode 序数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static ModeEnum forIndex(int mode) {
			for(var v: values()) if(v.ordinal() == mode) return v;
			return null;
		}
	}

	/**
	 * CTESTWINの時刻型を再現します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/06/12
	 */
	public static final class DateTime {
		private final ZonedDateTime epoch;

		/**
		 * 1970年1月1日を起点に時刻の解析器を構築します。
		 */
		public DateTime() {
			this.epoch = Instant.EPOCH.atZone(UTC);
		}

		/**
		 * 指定された時刻をデコードします。
		 *
		 * @param bits 時刻のビット列
		 *
		 * @return 日時
		 */
		public final Time decode(long binary) {
			final var b = Long.reverseBytes(binary);
			return new Time(epoch.plus(b, SECONDS));
		}

		/**
		 * 指定された日時をエンコードします。
		 *
		 * @param field 日時
		 *
		 * @return 時刻のビット列
		 */
		public final long encode(Time field) {
			final var f = field.value();
			final var v = epoch.until(f, SECONDS);
			return Long.reverseBytes(v);
		}
	}
}
