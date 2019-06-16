/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table.secret;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.time.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import qxsl.field.*;
import qxsl.model.*;

import static java.time.ZoneOffset.UTC;
import static java.time.temporal.ChronoUnit.DAYS;
import static java.time.temporal.ChronoUnit.MILLIS;

/**
 * zLogバイナリデータで交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/02/26
 *
 */
public final class ZBinFormat extends BaseFormat {
	private static final short USEUTC = 0x7FFF;

	/**
	 * 書式を構築します。
	 */
	public ZBinFormat() {
		super("zbin");
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return new ZBinDecoder(in).read();
	}

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		new ZBinEncoder(out).write(items);
	}

	/**
	 * Delphi言語のTDateTime型を閏秒を無視して再現します。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/23
	 *
	 */
	private static final class TDateTime {
		private final int MS_D = 86400000;
		private final ZonedDateTime epoch;

		/**
		 * 1899年12月30日を起点にTDateTime型を構築します。
		 *
		 * @param zoneId タイムゾーン
		 * @since 2019/05/15
		 */
		public TDateTime(ZoneId zoneId) {
			LocalDate date = LocalDate.of(1899, 12, 30);
			this.epoch = date.atStartOfDay(zoneId);
		}

		/**
		 * 指定されたTDateTimeを日時にデコードします。
		 * 
		 * @param led TDateTime型のビット列
		 * @return 日時
		 */
		public Time decode(long led) {
			final long bed = Long.reverseBytes(led);
			double d = Double.longBitsToDouble(bed);
			double t = Math.round(Math.abs(d) % 1 * MS_D);
			ZonedDateTime zdt = epoch.plus((int) d, DAYS);
			return new Time(zdt.plus((int) t, MILLIS));
		}

		/**
		 * 指定された日時をTDateTimeにエンコードします。
		 * 
		 * @param field 日時
		 * @return TDateTime型のビット列
		 */
		public long encode(Time field) {
			double ms = epoch.until(field.value(), MILLIS);
			final double time = Math.abs(ms) % MS_D / MS_D;
			final double date = ((long) ms) / MS_D + time;
			final long bit = Double.doubleToLongBits(date);
			return Long.reverseBytes(bit);
		}
	}

	/**
	 * zLogバイナリデータの周波数帯の列挙型です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/23
	 *
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
		private static BandEnum[] arr;

		private BandEnum(int kHz) {
			this.band = new Band(kHz);
		}

		@Override
		public String toString() {
			return band.toString();
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
		 * @return 対応する列挙子があれば返す
		 */
		public static BandEnum valueOf(Band band) {
			if(arr == null) arr = values();
			for(BandEnum b : arr) {
				if(b.band.equals(band)) return b;
			}
			return null;
		}

		/**
		 * 指定した序数に対応する列挙子を返します。
		 * 
		 * @param i 序数
		 * @return 対応する列挙子があれば返す
		 */
		public static BandEnum forIndex(int i) {
			if(arr == null) arr = values();
			for(BandEnum m : arr) {
				if(m.ordinal() == i) return m;
			}
			return null;
		}
	}

	/**
	 * zLogバイナリデータの通信方式の列挙型です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/23
	 *
	 */
	public enum ModeEnum {
		CW    ("CW"),
		SSB   ("SSB"),
		FM    ("FM"),
		AM    ("AM"),
		RTTY  ("RTTY"),
		OTHERS("Others");

		private final Mode mode;
		private static ModeEnum[] arr;

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
		 * @return 対応する列挙子があれば返す
		 */
		public static ModeEnum valueOf(Mode mode) {
			if(arr == null) arr = values();
			for(ModeEnum m : arr) {
				if(m.mode.equals(mode)) return m;
			}
			return null;
		}

		/**
		 * 指定した序数に対応する列挙子を返します。
		 * 
		 * @param i 序数
		 * @return 対応する列挙子があれば返す
		 */
		public static ModeEnum forIndex(int i) {
			if(arr == null) arr = values();
			for(ModeEnum m : arr) {
				if(m.ordinal() == i) return m;
			}
			return null;
		}
	}

	/**
	 * zLogバイナリデータの空中線出力の列挙型です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/23
	 *
	 */
	public enum WattEnum {
		P, L, M, H;

		private final Watt watt;
		private static WattEnum[] arr;

		private WattEnum() {
			watt = new Watt(name());
		}

		/**
		 * この列挙子に対応する出力を返します。
		 * 
		 * @return 出力
		 */
		public Watt toWatt() {
			return watt;
		}

		/**
		 * 指定された出力に対応する列挙子を返します。
		 * 
		 * @param watt 出力
		 * @return 対応する列挙子があれば返す
		 */
		public static WattEnum valueOf(Watt watt) {
			if(arr == null) arr = values();
			for(WattEnum p : arr) {
				if(p.watt.equals(watt)) return p;
			}
			return null;
		}

		/**
		 * 指定した序数に対応する列挙子を返します。
		 * 
		 * @param i 序数
		 * @return 対応する列挙子があれば返す
		 */
		public static WattEnum forIndex(int i) {
			if(arr == null) arr = values();
			for(WattEnum p : arr) {
				if(p.ordinal() == i) return p;
			}
			return null;
		}
	}

	/**
	 * zLogバイナリデータで直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/23
	 *
	 */
	private static final class ZBinDecoder {
		private final DataInputStream stream;
		private final Fields fields;
		private TDateTime tDateTime;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 */
		public ZBinDecoder(InputStream in) {
			this.fields = new Fields();
			this.stream = new DataInputStream(in);
		}

		/**
		 * 交信記録を読み込みます。ストリームは閉じられます。
		 * 
		 * @return 交信記録 交信記録がなければnull
		 * @throws IOException 入出力の例外
		 */
		public List<Item> read() throws IOException {
			try {
				return logSheet();
			} catch (IOException ex) {
				throw ex;
			} catch (Exception ex) {
				throw new IOException(ex);
			} finally {
				stream.close();
			}
		}

		/**
		 * 冒頭をスキップして交信記録を1件読み込みます。
		 * 
		 * @return 読み込んだ交信記録
		 * @throws Exception 読み込みに失敗した場合
		 */
		private List<Item> logSheet() throws Exception {
			this.tDateTime = new TDateTime(head());
			List<Item> items = new ArrayList<>();
			while(stream.available() > 0) items.add(item());
			return Collections.unmodifiableList(items);
		}

		/**
		 * ストリームからヘッダを読み込んでタイムゾーンを返します。
		 * 
		 * @return 読み込んだ{@link ZoneId}
		 * @throws IOException 読み込みに失敗した場合
		 * 
		 * @since 2019/05/14
		 */
		private ZoneId head() throws IOException {
			stream.readFully(new byte[0x54]);
			final short enoz = stream.readShort();
			short zone = Short.reverseBytes(enoz);
			stream.readFully(new byte[0xAA]);
			return zone == USEUTC? UTC: ZoneOffset.ofTotalSeconds(-60 * zone);
		}

		/**
		 * ストリームから{@link Item}を1件読み込みます。
		 * 
		 * @return 読み込んだ{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private Item item() throws Exception {
			final Item item = new Item();
			time(item);
			call(item);
			sent(item);
			rcvd(item);
			stream.skipBytes(1);
			sRSTQ(item);
			rRSTQ(item);
			stream.skipBytes(4);
			mode(item);
			band(item);
			watt(item);
			stream.skipBytes(65);
			oprt(item);
			note(item);
			stream.skipBytes(14);
			return item;
		}

		/**
		 * {@link Item}に交信日時を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void time(Item item) throws Exception {
			item.set(tDateTime.decode(stream.readLong()));
		}

		/**
		 * {@link Item}に相手局のコールサインを読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void call(Item item) throws Exception {
			final String s = readString(12);
			if(s != null) item.set(fields.cache(QxmlFields.CALL, s));
		}

		/**
		 * {@link Item}に相手局に送信したナンバーを読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void sent(Item item) throws Exception {
			final String s = readString(30);
			if(s != null) item.getSent().set(fields.cache(QxmlFields.CODE, s));
		}

		/**
		 * {@link Item}に相手局から受信したナンバーを読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rcvd(Item item) throws Exception {
			final String s = readString(30);
			if(s != null) item.getRcvd().set(fields.cache(QxmlFields.CODE, s));
		}

		/**
		 * {@link Item}に相手局に送信したRSTQを読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void sRSTQ(Item item) throws Exception {
			Short rst = Short.reverseBytes(stream.readShort());
			item.getSent().set(fields.cache(QxmlFields.RSTQ, rst.toString()));
		}

		/**
		 * {@link Item}に相手局から受信したRSTQを読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rRSTQ(Item item) throws Exception {
			Short rst = Short.reverseBytes(stream.readShort());
			item.getRcvd().set(fields.cache(QxmlFields.RSTQ, rst.toString()));
		}

		/**
		 * {@link Item}に通信方式を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void mode(Item item) throws Exception {
			item.set(ModeEnum.forIndex(stream.read()).toMode());
		}

		/**
		 * {@link Item}に周波数帯を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void band(Item item) throws Exception {
			item.set(BandEnum.forIndex(stream.read()).toBand());
		}

		/**
		 * {@link Item}に空中線出力を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void watt(Item item) throws Exception {
			item.getSent().set(WattEnum.forIndex(stream.read()).toWatt());
		}

		/**
		 * {@link Item}に運用者名を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void oprt(Item item) throws Exception {
			final String s = readString(14);
			if(s != null) item.set(fields.cache(QxmlFields.NAME, s));
		}

		/**
		 * {@link Item}に交信の備考を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void note(Item item) throws Exception {
			final String s = readString(66);
			if(s != null) item.set(fields.cache(QxmlFields.NOTE, s));
		}

		/**
		 * ストリームから指定された最大文字数の文字列を読み込みます。
		 * 
		 * @param limit 最大文字数
		 * @return 読み込んだ文字列
		 * @throws IOException 読み込みに失敗した場合
		 */
		private String readString(int limit) throws IOException {
			byte[] buff = new byte[stream.read()];
			stream.readFully(buff);
			stream.skipBytes(limit - buff.length);
			String val = new String(buff, "SJIS");
			return val.isEmpty()? null: val;
		}
	}

	/**
	 * 交信記録をzLogバイナリデータに直列化するエンコーダです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/23
	 *
	 */
	private static final class ZBinEncoder {
		private final TDateTime tDateTime;
		private final DataOutputStream stream;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 * 
		 * @param out 交信記録を書き込むストリーム
		 */
		public ZBinEncoder(OutputStream out) {
			this.tDateTime = new TDateTime(ZoneOffset.systemDefault());
			this.stream = new DataOutputStream(out);
		}

		/**
		 * 交信記録を出力します。ストリームは閉じられます。
		 * 
		 * @param items 交信記録
		 * @throws IOException 入出力の例外
		 */
		public void write(List<Item> items) throws IOException {
			final ZoneOffset zone = tDateTime.epoch.getOffset();
			final int secs = zone.getTotalSeconds();
			final int bits = secs == 0? USEUTC: secs / -60;
			stream.write(new byte[0x54]);
			stream.writeShort(Short.reverseBytes((short) bits));
			stream.write(new byte[0xAA]);
			for(Item r : items) item(r);
			stream.close();
		}

		/**
		 * {@link Item}をバイナリにシリアライズして出力します。
		 * 
		 * @param item 出力する{@link Item}
		 * @throws IOException 出力に失敗した場合
		 */
		private void item(Item item) throws IOException {
			int i = 0;
			time(item.get(Time.class));
			string(12, item.get(Call.class));
			string(30, item.getSent().get(Code.class));
			string(30, item.getRcvd().get(Code.class));
			while(i++ < 1) stream.writeByte(0);
			rst(item.getSent().get(RSTQ.class));
			rst(item.getRcvd().get(RSTQ.class));
			while(i++ < 6) stream.writeByte(0);
			mode(item.get(Mode.class));
			band(item.get(Band.class));
			watt(item.getSent().get(Watt.class));
			while(i++ < 72) stream.writeByte(0);
			string(14, item.get(Name.class));
			string(66, item.get(Note.class));
			while(i++ < 87) stream.writeByte(0);
			stream.flush();
		}

		/**
		 * 交信日時をTDateTime型で読めるバイト列に変換して出力します。
		 * 
		 * @param time 交信日時
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time time) throws IOException {
			if(time == null) stream.writeLong(0);
			else stream.writeLong(tDateTime.encode(time));
		}

		/**
		 * RSTQシグナルレポートを下位バイト、上位バイトの順に出力します。
		 * 
		 * @param rst RSTQシグナルレポート
		 * @throws IOException 出力に失敗した場合
		 */
		private void rst(RSTQ rst) throws IOException {
			int s = rst == null? 599 : rst.value();
			stream.writeShort(Short.reverseBytes((short) s));
		}

		/**
		 * 交信した通信方式を1バイトで出力します。
		 * 
		 * @param mode 通信方式
		 * @throws IOException 出力に失敗した場合
		 */
		private void mode(Mode mode) throws IOException {
			ModeEnum modes = ModeEnum.valueOf(mode);
			if(mode == null) stream.writeByte(0);
			else stream.writeByte(modes.ordinal());
		}

		/**
		 * 交信した周波数帯を1バイトで出力します。
		 * 
		 * @param band 周波数帯
		 * @throws IOException 出力に失敗した場合
		 */
		private void band(Band band) throws IOException {
			BandEnum bands = BandEnum.valueOf(band);
			if(bands == null) stream.writeByte(0);
			else stream.writeByte(bands.ordinal());
		}

		/**
		 * 交信時の空中線出力を1バイトで出力します。
		 * 
		 * @param watt 空中線出力
		 * @throws IOException 出力に失敗した場合
		 */
		private void watt(Watt watt) throws IOException {
			WattEnum watts = WattEnum.valueOf(watt);
			if(watts == null) stream.writeByte(0);
			else stream.writeByte(watts.ordinal());
		}

		/**
		 * 指定された属性を指定された最大文字数で出力します。
		 * 属性値が最大文字数を超過しても例外は発生しません。
		 *
		 * @param limit 最大文字数
		 * @param f 直列化する属性
		 * @throws IOException 出力に失敗した場合
		 */
		private void string(int limit, Field f) throws IOException {
			final String value = f != null? f.value().toString() : "";
			final byte[] bytes = value.getBytes("SJIS");
			stream.writeByte(bytes.length);
			stream.write(Arrays.copyOf(bytes, limit));
		}
	}
}
