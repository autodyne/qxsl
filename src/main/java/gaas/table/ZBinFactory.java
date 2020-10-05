/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.InputStream;
import java.io.OutputStream;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

import qxsl.table.BasicFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

import gaas.field.Band;
import gaas.field.Mode;
import gaas.field.Time;
import gaas.field.Watt;

import static java.lang.Math.abs;
import static java.lang.Math.round;
import static java.time.ZoneOffset.UTC;
import static java.time.temporal.ChronoUnit.DAYS;
import static java.time.temporal.ChronoUnit.MILLIS;

/**
 * zLogバイナリファイルの書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/26
 */
public final class ZBinFactory extends BasicFactory {
	/**
	 * 書式を構築します。
	 */
	public ZBinFactory() {
		super("zbin");
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
		return new ZBinDecoder(is, this);
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
		return new ZBinEncoder(os, this);
	}

	/**
	 * zLogバイナリデータの周波数帯の列挙型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/23
	 */
	public enum BandEnum {
		M1_9  (    1900),
		M3_5  (    3500),
		M7    (    7000),
		M10   (   10000),
		M14   (   14000),
		M18   (   18000),
		M21   (   21000),
		M24   (   24000),
		M28   (   28000),
		M50   (   50000),
		M144  (  144000),
		M430  (  430000),
		M1200 ( 1200000),
		M2400 ( 2400000),
		M5600 ( 5600000),
		G10UP (10000000);

		private final Band band;

		/**
		 * 周波数帯を指定して列挙子を生成します。
		 *
		 *
		 * @param band 周波数帯
		 */
		private BandEnum(int band) {
			this.band = new Band(band);
		}

		@Override
		public final String toString() {
			return band.toString();
		}

		/**
		 * この列挙子に対応する周波数帯を返します。
		 *
		 *
		 * @return 周波数帯
		 */
		public final Band toBand() {
			return band;
		}

		/**
		 * 指定された周波数帯に対応する列挙子を返します。
		 *
		 *
		 * @param band 周波数帯
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static final BandEnum valueOf(Band band) {
			for(var v: values()) if(v.band.equals(band)) return v;
			return null;
		}

		/**
		 * 指定された序数に対応する列挙子を返します。
		 *
		 *
		 * @param i 序数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static final BandEnum forIndex(int band) {
			for(var v: values()) if(v.ordinal() == band) return v;
			return null;
		}
	}

	/**
	 * zLogバイナリデータの通信方式の列挙型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/23
	 */
	public enum ModeEnum {
		CW    ("CW"),
		SSB   ("SSB"),
		FM    ("FM"),
		AM    ("AM"),
		RTTY  ("RTTY"),
		OTHER ("Other");

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
		 *
		 * @return モード
		 */
		public final Mode toMode() {
			return mode;
		}

		/**
		 * 指定されたモードに対応する列挙子を返します。
		 *
		 *
		 * @param mode モード
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static final ModeEnum valueOf(Mode mode) {
			for(var v: values()) if(v.mode.equals(mode)) return v;
			return ModeEnum.OTHER;
		}

		/**
		 * 指定された序数に対応する列挙子を返します。
		 *
		 *
		 * @param mode 序数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static final ModeEnum forIndex(int mode) {
			for(var v: values()) if(v.ordinal() == mode) return v;
			return null;
		}
	}

	/**
	 * zLogバイナリデータの送信電力の列挙型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/23
	 */
	public enum WattEnum {
		P, L, M, H;

		private final Watt watt;

		/**
		 * 送信電力を指定して列挙子を生成します。
		 *
		 *
		 * @param watt 送信電力
		 */
		private WattEnum() {
			watt = new Watt(name());
		}

		/**
		 * この列挙子に対応する出力を返します。
		 *
		 *
		 * @return 出力
		 */
		public final Watt toWatt() {
			return watt;
		}

		/**
		 * 指定された出力に対応する列挙子を返します。
		 *
		 *
		 * @param watt 出力
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static final WattEnum valueOf(Watt watt) {
			for(var v: values()) if(v.watt.equals(watt)) return v;
			return null;
		}

		/**
		 * 指定された序数に対応する列挙子を返します。
		 *
		 *
		 * @param watt 序数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static final WattEnum forIndex(int watt) {
			for(var v: values()) if(v.ordinal() == watt) return v;
			return null;
		}
	}

	/**
	 * Delphi言語の時刻型を閏秒を無視して再現します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/23
	 */
	public static final class DateTime {
		private static final long MS_DAY = 86400000;
		private static final long USEUTC = 0x7FFF;
		private final ZonedDateTime epoch;

		/**
		 * 1899年12月30日を起点に時刻の解析器を構築します。
		 *
		 *
		 * @since 2020/09/07
		 */
		public DateTime() {
			this(ZoneOffset.systemDefault());
		}

		/**
		 * 1899年12月30日を起点に時刻の解析器を構築します。
		 *
		 *
		 * @param zone 交信記録に適用する時間帯
		 *
		 * @since 2019/05/15
		 */
		public DateTime(ZoneId zone) {
			this.epoch = LocalDate.of(1899, 12, 30).atStartOfDay(zone);
		}

		/**
		 * 交信記録に適用する時間帯を返します。
		 *
		 *
		 * @return 時間帯
		 *
		 * @since 2020/09/07
		 */
		public final short getOffset() {
			final var zone = this.epoch.getOffset();
			final var secs = zone.getTotalSeconds();
			return Short.reverseBytes((short) (-secs / 60));
		}

		/**
		 * 指定された時刻をデコードします。
		 *
		 *
		 * @param data 時刻のビット列
		 *
		 * @return 日時
		 */
		public final Time decode(long data) {
			final var b = Long.reverseBytes(data);
			final var d = Double.longBitsToDouble(b);
			final var t = round(abs(d) % 1 * MS_DAY);
			final var z = epoch.plus((long) d, DAYS);
			return new Time(z.plus((long) t, MILLIS));
		}

		/**
		 * 指定された日時をエンコードします。
		 *
		 *
		 * @param field 日時
		 *
		 * @return 時刻のビット列
		 */
		public final long encode(Time field) {
			final var m = epoch.until(field.value(), MILLIS);
			final var d = abs((double) m) % MS_DAY / MS_DAY;
			final var b = Double.doubleToLongBits(d + m / MS_DAY);
			return Long.reverseBytes(b);
		}

		/**
		 * 指定された時差に対応する時刻の解析器を返します。
		 *
		 *
		 * @param bits 時差
		 *
		 * @return 時刻の解析器
		 */
		public static final DateTime newInstance(short bits) {
			final var zone = Short.reverseBytes(bits);
			final var mins = -60 * zone;
			if(zone == DateTime.USEUTC) return new DateTime(UTC);
			return new DateTime(ZoneOffset.ofTotalSeconds(mins));
		}
	}
}
