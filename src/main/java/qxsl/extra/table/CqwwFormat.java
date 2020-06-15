/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.nio.charset.Charset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import qxsl.extra.field.*;
import qxsl.field.FieldFormats;
import qxsl.model.Item;

import static java.nio.charset.StandardCharsets.US_ASCII;
import static java.time.ZoneOffset.UTC;

/**
 * CabrilloのQSOテンプレートでCQWWコンテストの交信記録を表す書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/04
 */
public final class CqwwFormat extends BaseFormat {
	public CqwwFormat() {
		super("cqww");
	}

	@Override
	public TableDecoder decoder(Reader reader) {
		return new CqwwDecoder(reader);
	}

	@Override
	public TableEncoder encoder(Writer writer) {
		return new CqwwEncoder(writer);
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		return decoder(new InputStreamReader(is, US_ASCII));
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		return encoder(new OutputStreamWriter(os, US_ASCII));
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
		private static BandEnum[] values;

		private BandEnum(String text, int kHz) {
			this.text = text;
			this.band = new Band(kHz);
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
			if(values == null) values = values();
			for(var b: values) if(b.band.equals(band)) return b;
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
			if(values == null) values = values();
			for(var b : values) if(b.text.equals(text)) return b;
			return null;
		}
	}

	/**
	 * Cabrillo書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/04
	 */
	private final class CqwwDecoder extends PlainTextDecoder {
		private final DateTimeFormatter format;
		private final FieldFormats fields;

		/**
		 * 指定されたリーダを読み込むデコーダを構築します。
		 *
		 * @param reader 更新小緑を読み込むリーダ
		 */
		public CqwwDecoder(Reader reader) {
			super(reader);
			fields = new FieldFormats();
			format = DateTimeFormatter.ofPattern("uuuu-MM-dd HHmm");
		}

		/**
		 * 交信記録を読み込みます。
		 *
		 * @return 交信記録
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
		 * 交信記録を読み込みます。
		 *
		 * @return 交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final List<Item> items() throws IOException {
			final List<Item> items = new ArrayList<>();
			String line;
			while((line = super.readLine()) != null) {
				if(!line.isBlank()) {
					super.reset();
					items.add(item(line));
				}
			}
			return Collections.unmodifiableList(items);
		}

		/**
		 * 1行の文字列から交信記録を1件読み込みます。
		 *
		 * @param line 1行
		 *
		 * @return 読み込んだ1件の交信
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private Item item(String line) throws IOException {
			final Item item = new Item();
			final String[] vals = splitLine(
				5, 11, 14, 30, 44, 48, 55, 69, 73, 80
			);

			final String band = vals[0];
			final String mode = vals[1];
			final String time = vals[2];
			final String srst = vals[4];
			final String snum = vals[5];
			final String call = vals[6];
			final String rrst = vals[7];
			final String rnum = vals[8];

			if(!time.isEmpty()) time(item, time);
			if(!call.isEmpty()) call(item, call);
			if(!srst.isEmpty()) srst(item, srst);
			if(!snum.isEmpty()) snum(item, snum);
			if(!rrst.isEmpty()) rrst(item, rrst);
			if(!rnum.isEmpty()) rnum(item, rnum);
			if(!band.isEmpty()) band(item, band);
			if(!mode.isEmpty()) mode(item, mode);

			return item;
		}

		/**
		 * 交信記録に交信日時を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param time 交信日時の文字列
		 */
		private void time(Item item, String time) {
			DateTimeFormatter format = this.format.withZone(UTC);
			item.add(new Time(ZonedDateTime.parse(time, format)));
		}

		/**
		 * 交信記録に相手局のコールサインを設定します。
		 *
		 * @param item 設定する交信記録
		 * @param call コールサインの文字列
		 */
		private void call(Item item, String call) {
			item.add(fields.cache(Qxsl.CALL).field(call));
		}

		/**
		 * 交信記録に相手局まで送信したRSTQを設定します。
		 *
		 * @param item 設定する交信記録
		 * @param srst RSTQの文字列
		 */
		private void srst(Item item, String srst) {
			item.getSent().add(fields.cache(Qxsl.RSTQ).field(srst));
		}

		/**
		 * 交信記録に相手局まで送信したナンバーを設定します。
		 *
		 * @param item 設定する交信記録
		 * @param snum ナンバーの文字列
		 */
		private void snum(Item item, String snum) {
			item.getSent().add(fields.cache(Qxsl.CODE).field(snum));
		}

		/**
		 * 交信記録に相手局から受信したRSTQを設定します。
		 *
		 * @param item 設定する交信記録
		 * @param rrst RSTQの文字列
		 */
		private void rrst(Item item, String rrst) {
			item.getRcvd().add(fields.cache(Qxsl.RSTQ).field(rrst));
		}

		/**
		 * 交信記録に相手局から受信したナンバーを設定します。
		 *
		 * @param item 設定する交信記録
		 * @param rnum ナンバーの文字列
		 */
		private void rnum(Item item, String rnum) {
			item.getRcvd().add(fields.cache(Qxsl.CODE).field(rnum));
		}

		/**
		 * 交信記録に周波数帯を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param band 周波数帯の文字列
		 */
		private void band(Item item, String band) {
			item.add(BandEnum.value(band.trim()).toBand());
		}

		/**
		 * 交信記録に通信方式を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param mode 通信方式の文字列
		 */
		private void mode(Item item, String mode) {
			item.add(fields.cache(Qxsl.MODE).field(mode));
		}
	}

	/**
	 * 交信記録をCabrillo書式に直列化するエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/04
	 */
	private final class CqwwEncoder extends PlainTextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたライタに出力するエンコーダを構築します。
		 *
		 * @param writer 交信記録を出力するライタ
		 */
		public CqwwEncoder(Writer writer) {
			super(writer);
			format = DateTimeFormatter.ofPattern("uuuu-MM-dd HHmm");
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
			for(Item r : items) item(r);
		}

		/**
		 * 指定された交信記録をテキスト書式で出力します。
		 *
		 * @param item 出力する交信記録
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private void item(Item item) throws IOException {
			print("QSO: ");
			band((Band) item.get(Qxsl.BAND));
			print(" ");
			printR(2, (Mode) item.get(Qxsl.MODE));
			print(" ");
			time((Time) item.get(Qxsl.TIME));
			print(" ");
			print("*************");
			print(" ");
			printR(3,  (RSTQ) item.getSent().get(Qxsl.RSTQ));
			print(" ");
			printR(6,  (Code) item.getSent().get(Qxsl.CODE));
			print(" ");
			printR(13, (Call) item.get(Qxsl.CALL));
			print(" ");
			printR(3,  (RSTQ) item.getRcvd().get(Qxsl.RSTQ));
			print(" ");
			printR(6,  (Code) item.getRcvd().get(Qxsl.CODE));
			println();
		}

		/**
		 * 指定された日時を文字列として出力します。
		 *
		 * @param date 出力する日時
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private void time(Time date) throws IOException {
			DateTimeFormatter format = this.format.withZone(UTC);
			if(date == null) print(" ".repeat(15));
			else print(format.format(date.value()));
		}

		/**
		 * 指定された周波数帯を文字列として出力します。
		 *
		 * @param band 出力する周波数帯
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private void band(Band band) throws IOException {
			BandEnum bands = BandEnum.valueOf(band);
			if(bands == null) print("     ");
			else printR(5, bands.toString());
		}
	}
}
