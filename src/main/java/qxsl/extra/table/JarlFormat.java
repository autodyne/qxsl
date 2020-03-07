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
 * JARLサマリーシートR2.0の交信記録を表現する書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2014/06/03
 *
 */
public final class JarlFormat extends BaseFormat {
	private final Charset SJIS = Charset.forName("SJIS");

	public JarlFormat() {
		super("jarl");
	}

	@Override
	public TableDecoder decoder(Reader reader) {
		return new JarlDecoder(reader);
	}

	@Override
	public TableEncoder encoder(Writer writer) {
		return new JarlEncoder(writer);
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
	 * JARL書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2014/06/03
	 */
	@Deprecated
	private final class JarlDecoder extends PlainTextDecoder {
		private final DateTimeFormatter format;
		private final FieldFormats fields;

		/**
		 * 指定されたリーダを読み込むデコーダを構築します。
		 *
		 * @param reader 交信記録を読み込むリーダ
		 */
		public JarlDecoder(Reader reader) {
			super(reader);
			fields = new FieldFormats();
			format = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm");
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
				if(line.startsWith("DATE")) continue;
				if(line.startsWith("DATA")) continue;
				if(line.startsWith("----")) continue;
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
			final String[] vs = line.split(" +", 11);
			final String time = vs[0].concat(" ").concat(vs[1]);
			final String band = vs[2];
			final String mode = vs[3];
			final String call = vs[4];
			final String srst = vs[5];
			final String snum = vs[6];
			final String rrst = vs[7];
			final String rnum = vs[8];

			if(!time.isEmpty()) time(item, time);
			if(!band.isEmpty()) band(item, band);
			if(!mode.isEmpty()) mode(item, mode);
			if(!call.isEmpty()) call(item, call);
			if(!srst.isEmpty()) srst(item, srst);
			if(!snum.isEmpty()) snum(item, snum);
			if(!rrst.isEmpty()) rrst(item, rrst);
			if(!rnum.isEmpty()) rnum(item, rnum);

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
		 * 交信記録に周波数帯を設定します。
		 *
		 * @param item 設定する交信記録
		 * @param band 周波数帯の文字列
		 */
		private void band(Item item, String band) {
			Integer kHz = (int) (Double.parseDouble(band) * 1000);
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

	}

	/**
	 * 交信記録をJARL書式に直列化するエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2014/06/03
	 */
	@Deprecated
	private final class JarlEncoder extends PlainTextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたライタに出力するエンコーダを構築します。
		 *
		 * @param writer 交信記録を出力するライタ
		 */
		public JarlEncoder(Writer writer) {
			super(writer);
			format = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm");
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
			for(Item r: items) item(r);
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
			band((Band) item.get(Qxsl.BAND));
			print(" ");
			printR(5,  (Mode) item.get(Qxsl.MODE));
			print(" ");
			printR(13, (Call) item.get(Qxsl.CALL));
			print(" ");
			printR(3,  (RSTQ) item.getSent().get(Qxsl.RSTQ));
			print(" ");
			printR(7,  (Code) item.getSent().get(Qxsl.CODE));
			print(" ");
			printR(3,  (RSTQ) item.getRcvd().get(Qxsl.RSTQ));
			print(" ");
			printR(7,  (Code) item.getRcvd().get(Qxsl.CODE));
			print(" ".repeat(10));
			print("1");
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
			final String MHz = band.toMHzString();
			printf("%5.5s", MHz.substring(0, MHz.length() - 3));
		}
	}
}
