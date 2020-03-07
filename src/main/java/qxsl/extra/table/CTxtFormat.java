/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.nio.charset.Charset;
import java.time.LocalDateTime;
import java.time.Year;
import java.time.format.*;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import qxsl.extra.field.*;
import qxsl.field.FieldFormats;
import qxsl.model.Item;

/**
 * 2016年4月以前のCTESTWINのテキストファイルの書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/07/02
 *
 */
public final class CTxtFormat extends BaseFormat {
	private final Charset SJIS = Charset.forName("SJIS");

	public CTxtFormat() {
		super("ctxt");
	}

	@Override
	public TableDecoder decoder(Reader reader) {
		return new CTxtDecoder(reader);
	}

	@Override
	public TableEncoder encoder(Writer writer) {
		return new CTxtEncoder(writer);
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
	 * CTESTWIN書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/07/02
	 */
	@Deprecated
	private final class CTxtDecoder extends PlainTextDecoder {
		private final DateTimeFormatter format;
		private final FieldFormats fields;

		/**
		 * 指定されたリーダを読み込むデコーダを構築します。
		 *
		 * @param reader 交信記録を読み込むリーダ
		 */
		public CTxtDecoder(Reader reader) {
			super(reader);
			fields = new FieldFormats();
			final var dtfb = new DateTimeFormatterBuilder();
			final var year = Year.now().getValue();
			dtfb.parseDefaulting(ChronoField.YEAR, year);
			dtfb.appendPattern("M/ppd HHmm");
			this.format = dtfb.toFormatter();
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
		 * 冒頭をスキップして交信記録を1件読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final List<Item> items() throws IOException {
			final List<Item> items = new ArrayList<>();
			String line;
			while((line = super.readLine()) != null) {
				if(!line.isBlank() && !line.startsWith("Worked")) {
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
		 * @return 読み込んだ1件の交信
		 * @throws IOException 読み込みに失敗した場合
		 */
		private Item item(String line) throws IOException {
			final Item item = new Item();
			final String[] vals = splitLine(0, 5, 16, 28, 36, 41, 54, 67);

			Integer.parseInt(vals[0]);

			final String time = vals[1];
			final String call = vals[2];
			final String band = vals[3];
			final String mode = vals[4];
			final String sent = vals[5];
			final String rcvd = vals[6];

			if(!time.isEmpty()) time(item, time);
			if(!call.isEmpty()) call(item, call);
			if(!band.isEmpty()) band(item, band);
			if(!mode.isEmpty()) mode(item, mode);
			if(!sent.isEmpty()) sent(item, sent);
			if(!rcvd.isEmpty()) rcvd(item, rcvd);

			return item;
		}

		/**
		 * {@link Item}に交信日時を設定します。
		 *
		 * @param item 設定する{@link Item}
		 * @param time 交信日時の文字列
		 * @throws IOException 読み込みに失敗した場合
		 */
		private void time(Item item, String time) throws IOException {
			item.add(new Time(LocalDateTime.parse(time, format)));
		}

		/**
		 * {@link Item}に相手局のコールサインを設定します。
		 *
		 * @param item 設定する{@link Item}
		 * @param call コールサインの文字列
		 * @throws IOException 読み込みに失敗した場合
		 */
		private void call(Item item, String call) throws IOException {
			item.add(fields.cache(Qxsl.CALL).field(call));
		}

		/**
		 * {@link Item}に周波数帯を設定します。
		 *
		 * @param item 設定する{@link Item}
		 * @param band 周波数帯の文字列
		 * @throws IOException 読み込みに失敗した場合
		 */
		private void band(Item item, String band) throws IOException {
			Integer kHz;
			if(band.contains("GHz")){
				band = band.replace("GHz", "");
				kHz = (int) (Double.parseDouble(band) * 1000_000);
			} else if(band.contains("MHz")) {
				band = band.replace("MHz", "");
				kHz = (int) (Double.parseDouble(band) * 1000);
			} else {
				band = band.replace("kHz", "");
				kHz = Integer.parseInt(band);
			}
			item.add(fields.cache(Qxsl.BAND).field(kHz.toString()));
		}

		/**
		 * {@link Item}に通信方式を設定します。
		 *
		 * @param item 設定する{@link Item}
		 * @param mode 通信方式の文字列
		 * @throws IOException 読み込みに失敗した場合
		 */
		private void mode(Item item, String mode) throws IOException {
			item.add(fields.cache(Qxsl.MODE).field(mode));
		}

		/**
		 * {@link Item}に相手局に送信したナンバーを設定します。
		 *
		 * @param item 設定する{@link Item}
		 * @param sent ナンバーの文字列
		 * @throws IOException 読み込みに失敗した場合
		 */
		private void sent(Item item, String sent) throws IOException {
			item.getSent().add(fields.cache(Qxsl.CODE).field(sent));
		}

		/**
		 * {@link Item}に相手局から受信したナンバーを設定します。
		 *
		 * @param item 設定する{@link Item}
		 * @param rcvd ナンバーの文字列
		 * @throws IOException 読み込みに失敗した場合
		 */
		private void rcvd(Item item, String rcvd) throws IOException {
			item.getRcvd().add(fields.cache(Qxsl.CODE).field(rcvd));
		}
	}

	/**
	 * 交信記録をCTESTWIN書式に直列化するエンコーダーです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/07/02
	 */
	private final class CTxtEncoder extends PlainTextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたライタに出力するエンコーダを構築します。
		 *
		 * @param writer 交信記録を出力するライタ
		 */
		public CTxtEncoder(Writer writer) {
			super(writer);
			format = DateTimeFormatter.ofPattern("MM/dd HHmm");
		}

		/**
		 * 交信記録を出力します。
		 *
		 * @param items 交信記録
		 * @throws IOException 出力に失敗した場合
		 */
		@Override
		public void encode(List<Item> items) throws IOException {
			printf("Worked %4s stations", items.size());
			println();
			println();
			int counter = 1;
			for(Item r : items) item(r, counter++);
		}

		/**
		 * 指定された交信記録をテキスト書式で出力します。
		 *
		 * @param item 出力する交信記録
		 * @param num 出力する交信記録の番号
		 *
		 * @throws IOException 出力に失敗した場合
		 */
		private void item(Item item, int num) throws IOException {
			printR(4, String.valueOf(num));
			print(" ");
			time((Time) item.get(Qxsl.TIME));
			print(" ");
			printR(11, (Call) item.get(Qxsl.CALL));
			print(" ");
			band((Band) item.get(Qxsl.BAND));
			print(" ");
			printR(4,  (Mode) item.get(Qxsl.MODE));
			print(" ");
			printR(12, (Code) item.getSent().get(Qxsl.CODE));
			print(" ");
			printR(12, (Code) item.getRcvd().get(Qxsl.CODE));
			println();
		}

		/**
		 * 指定された日時を文字列として出力します。
		 *
		 * @param date 出力する日時
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time date) throws IOException {
			if(date == null) print(" ".repeat(10));
			else print(format.format(date.value()));
		}

		/**
		 * 指定された周波数帯を文字列として出力します。
		 *
		 * @param band 出力する周波数帯
		 * @throws IOException 出力に失敗した場合
		 */
		private void band(Band band) throws IOException {
			printf("%-7.7s", band != null? band.toString() : "");
		}
	}
}
