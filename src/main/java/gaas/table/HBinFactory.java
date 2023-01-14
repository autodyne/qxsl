/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.InputStream;
import java.io.OutputStream;
import java.time.Year;
import java.time.ZoneOffset;

import qxsl.draft.Time;
import qxsl.table.BasicFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

import static java.time.ZoneOffset.UTC;

/**
 * HAMLOGの交信記録が従うHDB書式の互換実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/05
 */
public final class HBinFactory extends BasicFactory {
	/**
	 * 書式を構築します。
	 */
	public HBinFactory() {
		super("hbin");
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
		return new HBinDecoder(is);
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
		return new HBinEncoder(os);
	}

	/**
	 * HAMLOGの周波数帯の列挙型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2022/07/05
	 */
	public enum Column {
		CALLS ( 6),
		IGN   (14),
		DATE  ( 4),
		TIME  ( 2),
		CODE  ( 6),
		GL    ( 6),
		QSL   ( 3),
		FLAG  ( 2),
		HIS   ( 3),
		MY    ( 3),
		FREQ  ( 7),
		MODE  ( 4),
		NAME  (12),
		QTH   (28),
		RMK1  (54),
		RMK2  (54);

		private final int width;

		/**
		 * バイト数を指定して列挙子を生成します。
		 *
		 *
		 * @param width 周波数帯
		 */
		private Column(int width) {
			this.width = width;
		}

		/**
		 * この属性が占めるバイト数の初期値です。
		 *
		 */
		public int width() {
			return width;
		}
	}

	/**
	 * HAMLOGの時刻型を再現します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2022/06/25
	 */
	public static final class DateTime {
		private final ZoneOffset JST;

		/**
		 * 時刻型を準備します。
		 */
		public DateTime() {
			this.JST = ZoneOffset.ofHours(9);
		}

		/**
		 * 指定された時刻を解読します。
		 *
		 *
		 * @param binary 時刻のビット列
		 *
		 * @return 日時
		 */
		public final Time decode(byte[] bin) {
			final var zone = (bin[5] & 0x80) != 0? UTC: JST;
			final var year = Year.of(bin[0] * 100 + bin[1]);
			final var date = year.atMonth(bin[2]).atDay(bin[3]);
			final var time = date.atTime(bin[4], bin[5] & 0x7F);
			return new Time(time.atZone(zone));
		}

		/**
		 * 指定された日時をエンコードします。
		 *
		 *
		 * @param field 日時
		 *
		 * @return 時刻のビット列
		 */
		public final byte[] encode(Time field) {
			final var bin = new byte[6];
			final var off = field.value().getOffset();
			final var set = UTC.equals(off)? UTC: JST;
			final var val = field.atZone(set).value();
			final var utc = set.equals(UTC)? 0x80: 0;
			bin[0] = (byte) (val.getYear() / 100);
			bin[1] = (byte) (val.getYear() % 100);
			bin[2] = (byte) (val.getMonthValue());
			bin[3] = (byte) (val.getDayOfMonth());
			bin[4] = (byte) (val.getHour());
			bin[5] = (byte) (val.getMinute() | utc);
			return bin;
		}
	}
}
