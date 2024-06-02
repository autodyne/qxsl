/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
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
 * CTESTWINの交信記録が従うLG8書式の互換実装です。
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
		return new CBinDecoder(is);
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
		return new CBinEncoder(os);
	}

	/**
	 * この書式が対応する周波数帯の集合を返します。
	 *
	 *
	 * @return 周波数帯の集合
	 *
	 * @since 2024/06/02
	 */
	public static final FieldSet<Band> getBandSet() {
		final var set = new FieldSet<Band>("bands");
		set.add(new Band(     1900));
		set.add(new Band(     3500));
		set.add(new Band(     7000));
		set.add(new Band(    10000));
		set.add(new Band(    14000));
		set.add(new Band(    18000));
		set.add(new Band(    21000));
		set.add(new Band(    24000));
		set.add(new Band(    28000));
		set.add(new Band(    50000));
		set.add(new Band(   144000));
		set.add(new Band(   430000));
		set.add(new Band(  1200000));
		set.add(new Band(  2400000));
		set.add(new Band(  5600000));
		set.add(new Band( 10000000));
		set.add(new Band( 24000000));
		set.add(new Band( 47000000));
		set.add(new Band( 75000000));
		set.add(new Band( 77000000));
		set.add(new Band(135000000));
		set.add(new Band(248000000));
		set.add(new Band(      136));
		return set;
	}

	/**
	 * この書式が対応する通信方式の集合を返します。
	 *
	 *
	 * @return 通信方式の集合
	 *
	 * @since 2024/06/02
	 */
	public static final FieldSet<Mode> getModeSet() {
		final var set = new FieldSet<Mode>("modes");
		set.add(new Mode("CW"));
		set.add(new Mode("RTTY"));
		set.add(new Mode("SSB"));
		set.add(new Mode("FM"));
		set.add(new Mode("AM"));
		set.add(new Mode("ATV"));
		set.add(new Mode("SSTV"));
		set.add(new Mode("PSK"));
		set.add(new Mode("GMSK"));
		set.add(new Mode("MFSK"));
		set.add(new Mode("QPSK"));
		set.add(new Mode("FSK"));
		set.add(new Mode("D-STAR"));
		set.add(new Mode("C4FM"));
		set.add(new Mode("JT65"));
		set.add(new Mode("JT9"));
		set.add(new Mode("ISCAT"));
		set.add(new Mode("FT8"));
		set.add(new Mode("JT4"));
		set.add(new Mode("QRA64"));
		set.add(new Mode("MSK144"));
		set.add(new Mode("WSPR"));
		set.add(new Mode("JTMS"));
		set.add(new Mode("FT4"));
		return set;
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
		 * 指定された時刻を解読します。
		 *
		 *
		 * @param binary 時刻のビット列
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
