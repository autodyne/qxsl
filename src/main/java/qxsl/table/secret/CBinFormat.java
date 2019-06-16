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
import static java.time.temporal.ChronoUnit.SECONDS;

/**
 * CTESTWINのLG8書式で交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/06/12
 *
 */
public final class CBinFormat extends BaseFormat {
	/**
	 * 書式を構築します。
	 */
	public CBinFormat() {
		super("cbin");
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return new CBinDecoder(in).read();
	}

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		new CBinEncoder(out).write(items);
	}

	/**
	 * CTESTWINのCDateTime型を再現します。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2017/06/12
	 *
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
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2017/06/12
	 *
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
	 * LG8書式の通信方式の列挙型です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2017/06/12
	 *
	 */
	public enum ModeEnum {
		CW  ("CW"),
		RTTY("RTTY"),
		SSB ("SSB"),
		FM  ("FM"),
		AM  ("AM"),
		ATV ("ATV"),
		SSTV("SSTV"),
		PSK ("PSK"),
		GMSK("GMSK"),
		MFSK("MFSK"),
		QPSK("QPSK"),
		FSK ("FSK"),
		DSTAR("D-STAR"),
		C4FM("C4FM");

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
	 * LG8書式で直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2017/06/12
	 *
	 */
	private static final class CBinDecoder {
		private final Fields fields;
		private final CDateTime cDTime;
		private final DataInputStream stream;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 */
		public CBinDecoder(InputStream in) {
			this.fields = new Fields();
			this.cDTime = new CDateTime();
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
			List<Item> items = new ArrayList<>();
			final short hdr = stream.readShort();
			final short rdh = Short.reverseBytes(hdr);
			final long num = Short.toUnsignedInt(rdh);
			stream.skipBytes(6);
			byte[] tested = new byte[8];
			byte[] answer = "CQsoData".getBytes("ASCII");
			stream.readFully(tested);
			if(Arrays.equals(tested, answer)) {
				for(int i=0; i<num; i++) items.add(item());
				return Collections.unmodifiableList(items);
			} else throw new IOException("malformed data");
		}

		/**
		 * ストリームから{@link Item}を1件読み込みます。
		 * 
		 * @return 読み込んだ{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private Item item() throws Exception {
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
		 * {@link Item}に交信日時を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void time(Item item) throws Exception {
			item.set(cDTime.decode(stream.readLong()));
		}

		/**
		 * {@link Item}に相手局のコールサインを読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void call(Item item) throws Exception {
			final String s = readString(20);
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
		 * {@link Item}に運用者名を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void oprt(Item item) throws Exception {
			final String s = readString(20);
			if(s != null) item.set(fields.cache(QxmlFields.NAME, s));
		}

		/**
		 * {@link Item}に交信の備考を読み込みます。
		 * 
		 * @param item 設定する{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void note(Item item) throws Exception {
			final String s = readString(50);
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
			byte[] buff = new byte[limit];
			stream.readFully(buff);
			final String raw = new String(buff, "SJIS");
			final int len = raw.indexOf(0);
			if(len < 0) return raw;
			return len > 0? raw.substring(0, len): null;
		}
	}

	/**
	 * 交信記録をLG8書式に直列化するエンコーダです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2019/05/04
	 *
	 */
	private static final class CBinEncoder {
		private final CDateTime cDTime;
		private final DataOutputStream stream;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 * 
		 * @param out 交信記録を書き込むストリーム
		 */
		public CBinEncoder(OutputStream out) {
			this.cDTime = new CDateTime();
			this.stream = new DataOutputStream(out);
		}

		/**
		 * 交信記録を出力します。ストリームは閉じられます。
		 * 
		 * @param items 交信記録
		 * @throws IOException 入出力の例外
		 */
		public void write(List<Item> items) throws IOException {
			final short size = (short) items.size();
			stream.writeShort(Short.reverseBytes(size));
			stream.writeShort(0xFFFF);
			stream.writeShort(0x0000);
			stream.writeShort(0x0800);
			stream.writeBytes("CQsoData");
			short count = 0;
			for(Item r: items) item(r, ++count == size);
			stream.write(new byte[704]);
			// footer 704 bytes:
			// (1)   2 bytes: console ModeEnum
			// (2)   4 bytes: unknown
			// (3)   2 bytes: console BandEnum
			// (4)   2 bytes: contest ID
			// (5)   2 bytes: global score multiplier
			// (6)  92 bytes: 23band score multiplier
			// (7) 600 bytes: operator names (max 30)
			stream.close();
		}

		/**
		 * {@link Item}をバイナリにシリアライズして出力します。
		 * 
		 * @param item 出力する{@link Item}
		 * @param last 以降に交信記録がない場合true
		 * 
		 * @throws IOException 出力に失敗した場合
		 */
		private void item(Item item, boolean last) throws IOException {
			string(20, item.get(Call.class));
			string(30, item.getSent().get(Code.class));
			string(30, item.getRcvd().get(Code.class));
			mode(item.get(Mode.class));
			stream.writeByte(0);
			band(item.get(Band.class));
			stream.write(new byte[5]);
			time(item.get(Time.class));
			string(20, item.get(Name.class));
			stream.writeByte(0);
			stream.writeByte(0);
			string(50, item.get(Note.class));
			if(!last) stream.writeShort(0x0180);
			stream.flush();
		}

		/**
		 * 交信日時をCDateTime型で読めるバイト列に変換して出力します。
		 * 
		 * @param time 交信日時
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time time) throws IOException {
			if(time == null) stream.writeLong(0);
			else stream.writeLong(cDTime.encode(time));
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
			stream.write(Arrays.copyOf(bytes, limit-1));
			stream.writeByte(0);
		}
	}
}
