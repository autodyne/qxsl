/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.nio.charset.Charset;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import qxsl.extra.field.*;
import qxsl.field.FieldFormats;
import qxsl.model.Item;

/**
 * zLogテキストファイルのうちALLと呼ばれる書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/23
 *
 */
public final class ZAllFormat extends BaseFormat {
	private final Charset SJIS = Charset.forName("SJIS");

	public ZAllFormat() {
		super("zall");
	}

	@Override
	public TableDecoder decoder(Reader reader) {
		return new ZAllDecoder(reader);
	}

	@Override
	public TableEncoder encoder(Writer writer) {
		return new ZAllEncoder(writer);
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
	 * zLog ALL書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/23
	 */
	@Deprecated
	private final class ZAllDecoder extends PlainTextDecoder {
		private final DateTimeFormatter format;
		private final FieldFormats fields;

		/**
		 * 指定されたリーダを読み込むデコーダを構築します。
		 *
		 * @param reader 交信記録を読み込むリーダ
		 */
		public ZAllDecoder(Reader reader) {
			super(reader);
			fields = new FieldFormats();
			format = DateTimeFormatter.ofPattern("uuuu/MM/dd HH:mm");
		}

		/**
		 * 交信記録を読み込みます。
		 *
		 * @return 交信記録
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
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final List<Item> items() throws IOException {
			final List<Item> items = new ArrayList<>();
			String line;
			while((line = super.readLine()) != null) {
				if(line.isBlank()) continue;
				if(line.startsWith("zLog")) continue;
				if(line.startsWith("Date")) continue;
				super.reset();
				items.add(item(line));
			}
			return Collections.unmodifiableList(items);
		}

		/**
		 * 1行の文字列から交信記録を1件読み込みます。
		 *
		 * @param line 1行
		 * @return 読み込んだ1件の交信
		 * @throws IOException 読み込みに失敗した場合
		 */
		private Item item(String line) throws IOException {
			final Item item = new Item();
			final String[] vals = splitLine(
				0, 17, 30, 34, 42, 46, 54, 66, 71, 76, 79, 146
			);

			String time = vals[0];
			String call = vals[1];
			String srst = vals[2];
			String snum = vals[3];
			String rrst = vals[4];
			String rnum = vals[5];
			String band = vals[7];
			String mode = vals[8];
			String note = vals[10];

			final int i = note.indexOf("%%", 2);
			String oprt = i>0 ? note.substring(2, i) : "";
			if(i > 0) note = note.substring(i + 2).trim();

			if(!time.isEmpty()) time(item, time);
			if(!call.isEmpty()) call(item, call);
			if(!srst.isEmpty()) srst(item, srst);
			if(!snum.isEmpty()) snum(item, snum);
			if(!rrst.isEmpty()) rrst(item, rrst);
			if(!rnum.isEmpty()) rnum(item, rnum);
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
			item.add(new Time(LocalDateTime.parse(time, format)));
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
			Integer kHz;
			if(band.matches("^[0-9]+[gG]$")) {
				band = band.replaceAll("[gG]", "");
				kHz = (int) (Double.parseDouble(band) * 1000_000);
			} else {
				kHz = (int) (Double.parseDouble(band) * 1000);
			}
			item.add(fields.cache(Qxsl.BAND).field(kHz.toString()));
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

		/**
		 * 交信記録に運用者名を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param op 運用者名の文字列
		 */
		private void oprt(Item item, String op) {
			item.add(fields.cache(Qxsl.NAME).field(op));
		}

		/**
		 * 交信記録に交信の備考を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param note 備考の文字列
		 */
		private void note(Item item, String note) {
			item.add(fields.cache(Qxsl.NOTE).field(note));
		}
	}

	/**
	 * 交信記録をzLog ALL書式に直列化するエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/23
	 */
	@Deprecated
	private final class ZAllEncoder extends PlainTextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたライタに出力するエンコーダを構築します。
		 *
		 * @param writer 交信記録を出力するライタ
		 */
		public ZAllEncoder(Writer writer) {
			super(writer);
			format = DateTimeFormatter.ofPattern("uuuu/MM/dd HH:mm");
		}

		/**
		 * 交信記録を出力します。
		 *
		 * @param items 交信記録
		 * @throws IOException 出力に失敗した場合
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
		 * @throws IOException 出力に失敗した場合
		 */
		private void item(Item item) throws IOException {
			time((Time) item.get(Qxsl.TIME));
			print(" ");
			printL(12, (Call) item.get(Qxsl.CALL));
			print(" ");
			printL(3,  (RSTQ) item.getSent().get(Qxsl.RSTQ));
			print(" ");
			printL(7,  (Code) item.getSent().get(Qxsl.CODE));
			print(" ");
			printL(3,  (RSTQ) item.getRcvd().get(Qxsl.RSTQ));
			print(" ");
			printL(7,  (Code) item.getRcvd().get(Qxsl.CODE));
			print(" -     -     ");
			band((Band) item.get(Qxsl.BAND));
			print(" ");
			printL(4, (Mode) item.get(Qxsl.MODE));
			print(" 1  ");
			oprt((Name) item.get(Qxsl.NAME));
			note((Note) item.get(Qxsl.NOTE));
			println();
		}

		/**
		 * 指定された日時を文字列として出力します。
		 *
		 * @param date 出力する日時
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time date) throws IOException {
			if(date == null) print(" ".repeat(16));
			else print(format.format(date.value()));
		}

		/**
		 * 指定された周波数帯を文字列として出力します。
		 *
		 * @param band 出力する周波数帯
		 * @throws IOException 出力に失敗した場合
		 */
		private void band(Band band) throws IOException {
			if(band.value().intValueExact() < 10_000_000) {
				final String MHz = band.toMHzString();
				printf("%4.4s", MHz.substring(0, MHz.length() - 3));
			} else print(" 10G");
		}

		/**
		 * 指定された運用者名を文字列として出力します。
		 *
		 * @param op 出力する運用者名
		 * @throws IOException 出力に失敗した場合
		 */
		private void oprt(Name op) throws IOException {
			if(op != null) printf("%%%%%s%%%% ", op.value());
		}

		/**
		 * 指定された備考を文字列として出力します。
		 *
		 * @param note 出力する備考
		 * @throws IOException 出力に失敗した場合
		 */
		private void note(Note note) throws IOException {
			if(note != null) print(note.value());
		}
	}
}
