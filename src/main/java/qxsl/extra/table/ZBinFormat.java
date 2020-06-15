/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import qxsl.extra.field.*;
import qxsl.field.FieldFormats;
import qxsl.model.Field;
import qxsl.model.Item;

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
public final class ZBinFormat extends BaseFormat {
	private static final short USEUTC = 0x7FFF;

	public ZBinFormat() {
		super("zbin");
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		return new ZBinDecoder(is);
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		return new ZBinEncoder(os);
	}

	/**
	 * Delphi言語のTDateTime型を閏秒を無視して再現します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/23
	 */
	private static final class TDateTime {
		private final int MS_D = 86400000;
		private final ZonedDateTime epoch;

		/**
		 * 1899年12月30日を起点にTDateTime型を構築します。
		 *
		 * @param zoneId 交信記録に適用する時間帯
		 *
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
		 *
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
		 *
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
		private static BandEnum[] values;

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
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static BandEnum valueOf(Band band) {
			if(values == null) values = values();
			for(var b: values) if(b.band.equals(band)) return b;
			return null;
		}

		/**
		 * 指定された序数に対応する列挙子を返します。
		 *
		 * @param i 序数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static BandEnum forIndex(int i) {
			if(values == null) values = values();
			for(var b: values) if(b.ordinal() == i) return b;
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
		private static ModeEnum[] values;

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
			if(values == null) values = values();
			for(var m: values) if(m.mode.equals(mode)) return m;
			return ModeEnum.OTHER;
		}

		/**
		 * 指定された序数に対応する列挙子を返します。
		 *
		 * @param i 序数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static ModeEnum forIndex(int i) {
			if(values == null) values = values();
			for(var m: values) if(m.ordinal() == i) return m;
			return null;
		}
	}

	/**
	 * zLogバイナリデータの空中線出力の列挙型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/23
	 */
	public enum WattEnum {
		P, L, M, H;

		private final Watt watt;
		private static WattEnum[] values;

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
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static WattEnum valueOf(Watt watt) {
			if(values == null) values = values();
			for(var p: values) if(p.watt.equals(watt)) return p;
			return null;
		}

		/**
		 * 指定された序数に対応する列挙子を返します。
		 *
		 * @param i 序数
		 *
		 * @return 対応する列挙子があれば返す
		 */
		public static WattEnum forIndex(int i) {
			if(values == null) values = values();
			for(var p: values) if(p.ordinal() == i) return p;
			return null;
		}
	}

	/**
	 * zLogバイナリデータで直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/23
	 */
	private final class ZBinDecoder implements TableDecoder {
		private final DataInputStream stream;
		private final FieldFormats fields;
		private TDateTime tDTime;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 *
		 * @param is 交信記録を読み込むストリーム
		 */
		public ZBinDecoder(InputStream is) {
			this.fields = new FieldFormats();
			this.stream = new DataInputStream(is);
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException 解放の例外
		 */
		@Override
		public final void close() throws IOException {
			stream.close();
		}

		/**
		 * 冒頭をスキップして交信記録を1件読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		@Override
		public List<Item> decode() throws IOException {
			try {
				return items();
			} catch (RuntimeException ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * 冒頭をスキップして交信記録を1件読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final List<Item> items() throws IOException {
			this.tDTime = new TDateTime(head());
			final List<Item> items = new ArrayList<>();
			while(stream.available() > 0) items.add(item());
			return Collections.unmodifiableList(items);
		}

		/**
		 * ストリームからヘッダを読み込んで時間帯を返します。
		 *
		 * @return 読み込んだ時間帯
		 *
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
		 * ストリームから交信記録を1件読み込みます。
		 *
		 * @return 読み込んだ1件の交信
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final Item item() throws IOException {
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
		 * 交信記録に交信日時を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void time(Item item) throws IOException {
			item.add(tDTime.decode(stream.readLong()));
		}

		/**
		 * 交信記録に相手局のコールサインを読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void call(Item item) throws IOException {
			final String s = readString(12);
			item.add(fields.cache(Qxsl.CALL).field(s));
		}

		/**
		 * 交信記録に相手局まで送信したナンバーを読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void sent(Item item) throws IOException {
			final String s = readString(30);
			item.getSent().add(fields.cache(Qxsl.CODE).field(s));
		}

		/**
		 * 交信記録に相手局から受信したナンバーを読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void rcvd(Item item) throws IOException {
			final String s = readString(30);
			item.getRcvd().add(fields.cache(Qxsl.CODE).field(s));
		}

		/**
		 * 交信記録に相手局まで送信したRSTQを読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void sRSTQ(Item item) throws IOException {
			final var val = Short.reverseBytes(stream.readShort());
			final var rst = String.valueOf(val);
			item.getSent().add(fields.cache(Qxsl.RSTQ).field(rst));
		}

		/**
		 * 交信記録に相手局から受信したRSTQを読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void rRSTQ(Item item) throws IOException {
			final var val = Short.reverseBytes(stream.readShort());
			final var rst = String.valueOf(val);
			item.getRcvd().add(fields.cache(Qxsl.RSTQ).field(rst));
		}

		/**
		 * 交信記録に通信方式を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void mode(Item item) throws IOException {
			item.add(ModeEnum.forIndex(stream.read()).toMode());
		}

		/**
		 * 交信記録に周波数帯を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void band(Item item) throws IOException {
			item.add(BandEnum.forIndex(stream.read()).toBand());
		}

		/**
		 * 交信記録に空中線出力を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void watt(Item item) throws IOException {
			item.getSent().add(WattEnum.forIndex(stream.read()).toWatt());
		}

		/**
		 * 交信記録に運用者名を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void oprt(Item item) throws IOException {
			final String s = readString(14);
			item.add(fields.cache(Qxsl.NAME).field(s));
		}

		/**
		 * 交信記録に交信の備考を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void note(Item item) throws IOException {
			final String s = readString(66);
			item.add(fields.cache(Qxsl.NOTE).field(s));
		}

		/**
		 * ストリームから指定された最大文字数の文字列を読み込みます。
		 *
		 * @param limit 最大文字数
		 *
		 * @return 読み込んだ文字列
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private String readString(int limit) throws IOException {
			byte[] buff = new byte[stream.read()];
			stream.readFully(buff);
			stream.skipBytes(limit - buff.length);
			return new String(buff, "SJIS");
		}
	}

	/**
	 * 交信記録をzLogバイナリデータに直列化するエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/23
	 *
	 */
	private final class ZBinEncoder implements TableEncoder {
		private final TDateTime tDTime;
		private final DataOutputStream stream;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 *
		 * @param os 交信記録を出力するストリーム
		 */
		public ZBinEncoder(OutputStream os) {
			this.tDTime = new TDateTime(ZoneOffset.systemDefault());
			this.stream = new DataOutputStream(os);
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException 解放の例外
		 */
		@Override
		public final void close() throws IOException {
			stream.close();
		}

		/**
		 * 交信記録を出力します。
		 *
		 * @param items 交信記録
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		@Override
		public void encode(List<Item> items) throws IOException {
			final ZoneOffset zone = tDTime.epoch.getOffset();
			final int secs = zone.getTotalSeconds();
			final int bits = secs == 0? USEUTC: secs / -60;
			stream.write(new byte[0x54]);
			stream.writeShort(Short.reverseBytes((short) bits));
			stream.write(new byte[0xAA]);
			for(Item r : items) item(r);
		}

		/**
		 * 交信記録をバイナリにシリアライズして出力します。
		 *
		 * @param item 出力する交信記録
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private final void item(Item item) throws IOException {
			int i = 0;
			time((Time) item.get(Qxsl.TIME));
			string(12, (Call) item.get(Qxsl.CALL));
			string(30, (Code) item.getSent().get(Qxsl.CODE));
			string(30, (Code) item.getRcvd().get(Qxsl.CODE));
			while(i++ < 1) stream.writeByte(0);
			rst((RSTQ) item.getSent().get(Qxsl.RSTQ));
			rst((RSTQ) item.getRcvd().get(Qxsl.RSTQ));
			while(i++ < 6) stream.writeByte(0);
			mode((Mode) item.get(Qxsl.MODE));
			band((Band) item.get(Qxsl.BAND));
			watt((Watt) item.getSent().get(Qxsl.WATT));
			while(i++ < 72) stream.writeByte(0);
			string(14, (Name) item.get(Qxsl.NAME));
			string(66, (Note) item.get(Qxsl.NOTE));
			while(i++ < 87) stream.writeByte(0);
			stream.flush();
		}

		/**
		 * 交信日時をTDateTime型で読めるバイト列に変換して出力します。
		 *
		 * @param time 交信日時
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private final void time(Time time) throws IOException {
			if(time == null) stream.writeLong(0);
			else stream.writeLong(tDTime.encode(time));
		}

		/**
		 * RSTQシグナルレポートを下位バイト、上位バイトの順に出力します。
		 *
		 * @param rst RSTQシグナルレポート
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private final void rst(RSTQ rst) throws IOException {
			int s = rst == null? 599 : rst.value();
			stream.writeShort(Short.reverseBytes((short) s));
		}

		/**
		 * 交信した通信方式を1バイトで出力します。
		 *
		 * @param mode 通信方式
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private final void mode(Mode mode) throws IOException {
			stream.writeByte(ModeEnum.valueOf(mode).ordinal());
		}

		/**
		 * 交信した周波数帯を1バイトで出力します。
		 *
		 * @param band 周波数帯
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private final void band(Band band) throws IOException {
			BandEnum bands = BandEnum.valueOf(band);
			if(bands == null) stream.writeByte(0);
			else stream.writeByte(bands.ordinal());
		}

		/**
		 * 交信時の空中線出力を1バイトで出力します。
		 *
		 * @param watt 空中線出力
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private final void watt(Watt watt) throws IOException {
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
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private void string(int limit, Field f) throws IOException {
			final String value = f != null? f.value().toString() : "";
			final byte[] bytes = value.getBytes("SJIS");
			stream.writeByte(bytes.length);
			stream.write(Arrays.copyOf(bytes, limit));
		}
	}
}
