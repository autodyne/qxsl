/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

import qxsl.draft.Time;
import qxsl.table.BasicFactory;

import static java.lang.Math.abs;
import static java.lang.Math.round;
import static java.time.ZoneOffset.UTC;
import static java.time.temporal.ChronoUnit.DAYS;
import static java.time.temporal.ChronoUnit.MILLIS;

/**
 * zLogの交信記録が従うZLO/ZLOX書式の共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2024/06/02
 */
public abstract class ZLogFactory extends BasicFactory {
	/**
	 * 指定された名前の書式を構築します。
	 *
	 *
	 * @param format 書式の名前
	 */
	public ZLogFactory(String format) {
		super(format);
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
		private static final int MS_DAY = 86400000;
		private static final int USEUTC = 0x7FFF;
		private static final int USEJST = 0;
		private static final int SECS = -60;
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
			final var off = this.epoch.getOffset();
			final int min = off.getTotalSeconds() / SECS;
			final int val = off.equals(UTC)? USEUTC: min;
			return Short.reverseBytes((short) val);
		}

		/**
		 * 指定された時刻を解読します。
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
		 * @param time 日時
		 *
		 * @return 時刻のビット列
		 */
		public final long encode(Time time) {
			final var m = epoch.until(time.value(), MILLIS);
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
			final int min = Short.reverseBytes(bits);
			final var JST = ZoneId.of("JST", ZoneId.SHORT_IDS);
			if(min == DateTime.USEUTC) return new DateTime(UTC);
			if(min == DateTime.USEJST) return new DateTime(JST);
			return new DateTime(ZoneOffset.ofTotalSeconds(SECS * min));
		}
	}
}
