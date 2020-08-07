/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import qxsl.extra.field.*;
import qxsl.field.FieldFormats;
import qxsl.model.Field;
import qxsl.model.Item;

import java.io.*;
import java.time.LocalDate;
import java.time.Year;
import java.time.ZonedDateTime;
import java.util.*;

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
public final class CBinFormat extends BaseFormat {
	public CBinFormat() {
		super("cbin");
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		return new CBinDecoder(is);
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		return new CBinEncoder(os);
	}

	/**
	 * CTESTWINのCDateTime型を再現します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/06/12
	 */
	private static final class CDateTime {
		private final ZonedDateTime epoch;

		/**
		 * 1970年1月1日を起点にCDateTime型を構築します。
		 */
		public CDateTime() {
			LocalDate date = Year.of(1970).atDay(1);
			this.epoch = date.atStartOfDay(UTC);
		}

		/**
		 * 指定されたCDateTimeを日時にデコードします。
		 *
		 * @param led CDateTime型のビット列
		 *
		 * @return 日時
		 */
		public Time decode(long led) {
			final long bed = Long.reverseBytes(led);
			return new Time(epoch.plus(bed, SECONDS));
		}

		/**
		 * 指定された日時をCDateTimeにエンコードします。
		 *
		 * @param field 日時
		 *
		 * @return CDateTime型のビット列
		 */
		public long encode(Time field) {
			long ms = epoch.until(field.value(), SECONDS);
			return Long.reverseBytes(ms);
		}
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
			return null;
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
	 * LG8書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/06/12
	 */
	private final class CBinDecoder implements TableDecoder {
		private final FieldFormats fields;
		private final CDateTime cDTime;
		private final DataInputStream stream;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 *
		 * @param is 交信記録を読み込むストリーム
		 */
		public CBinDecoder(InputStream is) {
			this.fields = new FieldFormats();
			this.cDTime = new CDateTime();
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
		 * @return 交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final List<Item> items() throws IOException {
			final List<Item> items = new ArrayList<>();
			final short hdr = stream.readShort();
			final short rdh = Short.reverseBytes(hdr);
			final long num = Short.toUnsignedInt(rdh);
			stream.skipBytes(6);
			byte[] trial = new byte[8];
			byte[] truth = "CQsoData".getBytes("ASCII");
			stream.readFully(trial);
			if(Arrays.equals(trial, truth)) {
				for(int i=0; i<num; i++) items.add(item());
				return Collections.unmodifiableList(items);
			} else throw new IOException("malformed data");
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
			call(item);
			sent(item);
			rcvd(item);
			mode(item);
			stream.read();
			band(item);
			stream.skipBytes(5);
			time(item);
			oprt(item);
			stream.skipBytes(2);
			note(item);
			stream.skipBytes(2);
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
			item.set(cDTime.decode(stream.readLong()));
		}

		/**
		 * 交信記録に相手局のコールサインを読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void call(Item item) throws IOException {
			final String s = readString(20);
			item.set(fields.cache(Qxsl.CALL).field(s));
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
			item.getSent().set(fields.cache(Qxsl.CODE).field(s));
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
			item.getRcvd().set(fields.cache(Qxsl.CODE).field(s));
		}

		/**
		 * 交信記録に通信方式を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void mode(Item item) throws IOException {
			item.set(ModeEnum.forIndex(stream.read()).toMode());
		}

		/**
		 * 交信記録に周波数帯を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void band(Item item) throws IOException {
			item.set(BandEnum.forIndex(stream.read()).toBand());
		}

		/**
		 * 交信記録に運用者名を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void oprt(Item item) throws IOException {
			final String s = readString(20);
			item.set(fields.cache(Qxsl.NAME).field(s));
		}

		/**
		 * 交信記録に交信の備考を読み込みます。
		 *
		 * @param item 設定する交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final void note(Item item) throws IOException {
			final String s = readString(50);
			item.set(fields.cache(Qxsl.NOTE).field(s));
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
			byte[] buff = new byte[limit];
			stream.readFully(buff);
			final String raw = new String(buff, "SJIS");
			final int len = raw.indexOf(0);
			return raw.substring(0, Math.max(0, len));
		}
	}

	/**
	 * 交信記録をLG8書式に直列化するエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/04
	 */
	private final class CBinEncoder implements TableEncoder {
		private final CDateTime cDTime;
		private final DataOutputStream stream;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 *
		 * @param os 交信記録を出力するストリーム
		 */
		public CBinEncoder(OutputStream os) {
			this.cDTime = new CDateTime();
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
		 * @throws IOException 書き込みに失敗した場合
		 */
		@Override
		public void encode(List<Item> items) throws IOException {
			final short size = (short) items.size();
			stream.writeShort(Short.reverseBytes(size));
			stream.writeShort(0xFFFF);
			stream.writeShort(0x0000);
			stream.writeShort(0x0800);
			stream.writeBytes("CQsoData");
			short count = 0;
			for(Item r: items) item(r, ++count == size);
			confs(items.isEmpty()? List.of(new Item()): items);
			names(items.isEmpty()? List.of(new Item()): items);
		}

		/**
		 * CTESTWINの入力画面のデフォルト値を出力します。
		 *
		 * @param items 交信記録
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void confs(List<Item> items) throws IOException {
			final var last = items.get(items.size()-1);
			mode((Mode) last.get(Qxsl.MODE));
			stream.writeByte(0);
			stream.writeInt(0);
			band((Band) last.get(Qxsl.BAND));
			stream.writeByte(0);
			stream.writeByte(0); // contest ID
			stream.writeByte(0);
			scoresForAADX();
		}

		/**
		 * CTESTWINのAADXコンテスト用の点数設定を出力します。
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void scoresForAADX() throws IOException {
			final int BANDS = 46; // 23 * (Asia & Non-Asia)
			stream.writeShort(0x0000);   // Global Settings
			for(int i=0; i<BANDS; i++) stream.writeShort(0x0100);
		}

		/**
		 * CTESTWINの運用者名のリストの設定を出力します。
		 *
		 * @param items 交信記録
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void names(List<Item> items) throws IOException {
			final var names = new LinkedHashSet<Name>();
			for(Item item: items) {
				var name = (Name) item.get(Qxsl.NAME);
				if(names.size() < 30) names.add(name);
			}
			for(Field name: names) string(20, name);
			stream.write(new byte[600 - 20 * names.size()]);
		}

		/**
		 * 交信記録をバイナリにシリアライズして出力します。
		 *
		 * @param item 出力する交信記録
		 * @param last 以降に交信記録がない場合true
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void item(Item item, boolean last) throws IOException {
			string(20, (Call) item.get(Qxsl.CALL));
			string(30, (Code) item.getSent().get(Qxsl.CODE));
			string(30, (Code) item.getRcvd().get(Qxsl.CODE));
			mode((Mode) item.get(Qxsl.MODE));
			stream.writeByte(0);
			band((Band) item.get(Qxsl.BAND));
			stream.writeByte(0);
			stream.writeByte(0x0a);
			stream.writeByte(0);
			stream.writeByte(0);
			stream.writeByte(0x80);
			time((Time) item.get(Qxsl.TIME));
			string(20, (Name) item.get(Qxsl.NAME));
			stream.writeByte(0);
			stream.writeByte(0);
			string(50, (Note) item.get(Qxsl.NOTE));
			if(!last) stream.writeShort(0x0180);
			stream.flush();
		}

		/**
		 * 交信日時をCDateTime型で読めるバイト列に変換して出力します。
		 *
		 * @param time 交信日時
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private final void time(Time time) throws IOException {
			if(time == null) stream.writeLong(0);
			else stream.writeLong(cDTime.encode(time));
		}

		/**
		 * 交信した通信方式を1バイトで出力します。
		 *
		 * @param mode 通信方式
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private final void mode(Mode mode) throws IOException {
			ModeEnum modes = ModeEnum.valueOf(mode);
			if(mode == null) stream.writeByte(0);
			else stream.writeByte(modes.ordinal());
		}

		/**
		 * 交信した周波数帯を1バイトで出力します。
		 *
		 * @param band 周波数帯
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private final void band(Band band) throws IOException {
			BandEnum bands = BandEnum.valueOf(band);
			if(bands == null) stream.writeByte(0);
			else stream.writeByte(bands.ordinal());
		}

		/**
		 * 指定された属性を指定された最大文字数で出力します。
		 * 属性値が最大文字数を超過しても例外は発生しません。
		 *
		 * @param limit 最大文字数
		 * @param f 直列化する属性
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void string(int limit, Field f) throws IOException {
			final String value = f != null? f.value().toString() : "";
			final byte[] bytes = value.getBytes("SJIS");
			stream.write(Arrays.copyOf(bytes, limit-1));
			stream.writeByte(0);
		}
	}
}
