/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import qxsl.extra.field.*;
import qxsl.field.FieldFormats;
import qxsl.model.Item;

import java.io.*;
import java.nio.charset.Charset;
import java.time.LocalDateTime;
import java.time.Year;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * zLogテキストファイルのうちDOS版と互換の書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/27
 */
public final class ZDosFormat extends BaseFormat {
	private final Charset SJIS = Charset.forName("SJIS");

	public ZDosFormat() {
		super("zdos");
	}

	@Override
	public TableDecoder decoder(Reader reader) {
		return new ZDosDecoder(reader);
	}

	@Override
	public TableEncoder encoder(Writer writer) {
		return new ZDosEncoder(writer);
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		return decoder(new InputStreamReader(is, SJIS));
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		return encoder(new OutputStreamWriter(os, SJIS));
	}

	/**
	 * zLogテキスト書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/25
	 */
	@Deprecated
	private final class ZDosDecoder extends PlainTextDecoder {
		private final DateTimeFormatter format;
		private final FieldFormats fields;

		/**
		 * 指定されたリーダを読み込むデコーダを構築します。
		 *
		 * @param reader 交信記録を読み込むリーダ
		 */
		public ZDosDecoder(Reader reader) {
			super(reader);
			this.fields = new FieldFormats();
			final var dtfb = new DateTimeFormatterBuilder();
			final var year = Year.now().getValue();
			dtfb.parseDefaulting(ChronoField.YEAR, year);
			dtfb.appendPattern("M  ppd HHmm");
			this.format = dtfb.toFormatter();
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
		 * 冒頭をスキップして交信記録を1件読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final List<Item> items() throws IOException {
			final List<Item> items = new ArrayList<>();
			String line;
			while((line = super.readLine()) != null) {
				if(!line.isBlank() && !line.startsWith("mon")) {
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
			final String[] vals = split(
				0, 13, 24, 37, 50, 57, 63, 68, 72, 139
			);

			String time = vals[0];
			String call = vals[1];
			String sent = vals[2];
			String rcvd = vals[3];
			String band = vals[5];
			String mode = vals[6];
			String note = vals[8];

			final int i = note.indexOf("%%", 2);
			String oprt = i>0 ? note.substring(2, i) : "";
			if(i > 0) note = note.substring(i + 2).trim();

			if(!time.isEmpty()) time(item, time);
			if(!call.isEmpty()) call(item, call);
			if(!sent.isEmpty()) sent(item, sent);
			if(!rcvd.isEmpty()) rcvd(item, rcvd);
			if(!band.isEmpty()) band(item, band);
			if(!mode.isEmpty()) mode(item, mode);
			if(!oprt.isEmpty()) oprt(item, oprt);
			if(!note.isEmpty()) note(item, note);

			return item;
		}

		/**
		 * 交信記録に交信日時を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param time 交信日時の文字列
		 */
		private void time(Item item, String time) {
			item.set(new Time(LocalDateTime.parse(time, format)));
		}

		/**
		 * 交信記録に相手局のコールサインを設定します。
		 *
		 * @param item 設定する交信記録
		 * @param call コールサインの文字列
		 */
		private void call(Item item, String call) {
			item.set(fields.cache(Qxsl.CALL).field(call));
		}

		/**
		 * 交信記録に相手局まで送信したナンバーを設定します。
		 *
		 * @param item 設定する交信記録
		 * @param sent ナンバーの文字列
		 */
		private void sent(Item item, String sent) {
			item.getSent().set(fields.cache(Qxsl.CODE).field(sent));
		}

		/**
		 * 交信記録に相手局から受信したナンバーを設定します。
		 *
		 * @param item 設定する交信記録
		 * @param rcvd ナンバーの文字列
		 */
		private void rcvd(Item item, String rcvd) {
			item.getRcvd().set(fields.cache(Qxsl.CODE).field(rcvd));
		}

		/**
		 * 交信記録に周波数帯を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param band 周波数帯の文字列
		 */
		private void band(Item item, String band) {
			Integer kHz;
			if(band.matches("^[0-9]+[gG]$")) {
				band = band.replaceAll("[gG]", "");
				kHz = (int) (Double.parseDouble(band) * 1000_000);
			} else {
				kHz = (int) (Double.parseDouble(band) * 1000);
			}
			item.set(fields.cache(Qxsl.BAND).field(kHz.toString()));
		}

		/**
		 * 交信記録に通信方式を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param mode 通信方式の文字列
		 */
		private void mode(Item item, String mode) {
			item.set(fields.cache(Qxsl.MODE).field(mode));
		}

		/**
		 * 交信記録に運用者名を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param op 運用者名の文字列
		 */
		private void oprt(Item item, String op) {
			item.set(fields.cache(Qxsl.NAME).field(op));
		}

		/**
		 * 交信記録に交信の備考を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param note 備考の文字列
		 */
		private void note(Item item, String note) {
			item.set(fields.cache(Qxsl.NOTE).field(note));
		}
	}

	/**
	 * 交信記録をzLogテキスト書式に直列化するエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/25
	 */
	@Deprecated
	private final class ZDosEncoder extends PlainTextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたライタに出力するエンコーダを構築します。
		 *
		 * @param writer 交信記録を出力するライタ
		 */
		public ZDosEncoder(Writer writer) {
			super(writer);
			format = DateTimeFormatter.ofPattern(" MM  dd HHmm");
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
			print(getHeaderText());
			println();
			for(Item r : items) item(r);
		}

		/**
		 * 指定された交信記録をテキスト書式で出力します。
		 *
		 * @param item 出力する交信記録
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void item(Item item) throws IOException {
			time((Time) item.get(Qxsl.TIME));
			print(" ");
			printR(10, (Call) item.get(Qxsl.CALL));
			print(" ");
			printR(12, (Code) item.getSent().get(Qxsl.CODE));
			print(" ");
			printR(12, (Code) item.getRcvd().get(Qxsl.CODE));
			print("        ");
			band((Band) item.get(Qxsl.BAND));
			print(" ");
			printR(4, (Mode) item.get(Qxsl.MODE));
			print(" 1   ");
			oprt((Name) item.get(Qxsl.NAME));
			note((Note) item.get(Qxsl.NOTE));
			println();
		}

		/**
		 * 指定された日時を文字列として出力します。
		 *
		 * @param date 出力する日時
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void time(Time date) throws IOException {
			if(date == null) print(" ".repeat(12));
			else print(format.format(date.value()));
		}

		/**
		 * 指定された周波数帯を文字列として出力します。
		 *
		 * @param band 出力する周波数帯
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void band(Band band) throws IOException {
			if(band.value().intValueExact() < 10_000_000) {
				final String MHz = band.toMHzString();
				printf("%5.5s", MHz.substring(0, MHz.length() - 3));
			} else print("  10G");
		}

		/**
		 * 指定された運用者名を文字列として出力します。
		 *
		 * @param op 出力する運用者名
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void oprt(Name op) throws IOException {
			if(op != null) printf("%%%%%s%%%% ", op.value());
		}

		/**
		 * 指定された備考を文字列として出力します。
		 *
		 * @param note 出力する備考
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		private void note(Note note) throws IOException {
			if(note != null) print(note.value());
		}
	}
}
