/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.table;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
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
 * 2016年4月以前のHLTST書式で交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/20
 *
 */
public final class Hl76Format extends TextFormat {
	/**
	 * 書式を構築します。
	 */
	public Hl76Format() {
		super("hl76");
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return new Hl76Decoder(in).read();
	}

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		new Hl76Encoder(out).write(items);
	}

	/**
	 * HLTST書式で直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/20
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	@Deprecated
	private final class Hl76Decoder extends TextDecoder {
		private final DateTimeFormatter format;
		private final FieldFormats fields;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public Hl76Decoder(InputStream in) throws IOException {
			super(in, "JISAutoDetect");
			fields = new FieldFormats();
			DateTimeFormatterBuilder fb = new DateTimeFormatterBuilder();
			fb.parseDefaulting(ChronoField.YEAR, Year.now().getValue());
			this.format = fb.appendPattern("M/ppd HH:mm").toFormatter();
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
				throw parseError(ex);
			} finally {
				super.close();
			}
		}

		private List<Item> logSheet() throws Exception {
			List<Item> items = new ArrayList<>();
			String line;
			while((line = super.readLine()) != null) {
				if(line.isEmpty()) continue;
				if(line.startsWith("MM/DD")) continue;
				if(line.startsWith("-----")) continue;
				items.add(item(line));
			}
			return Collections.unmodifiableList(items);
		}

		/**
		 * 1行の文字列から{@link Item}を1件読み込みます。
		 * 
		 * @param line 1行
		 * @return 読み込んだ{@link Item}
		 * @throws Exception 読み込みに失敗した場合
		 */
		private Item item(String line) throws Exception {
			final Item item = new Item();
			final String time = subLine(0,  11);
			final String call = subLine(12, 22);
			final String srst = subLine(23, 26);
			final String snum = subLine(27, 34);
			final String rrst = subLine(35, 38);
			final String rnum = subLine(39, 46);
			final String oprt = subLine(56, 63);
			final String freq = subLine(64, 68);
			final String mode = subLine(69, -1);

			if(!time.isEmpty()) time(item, time);
			if(!call.isEmpty()) call(item, call);
			if(!srst.isEmpty()) srst(item, srst);
			if(!snum.isEmpty()) snum(item, snum);
			if(!rrst.isEmpty()) rrst(item, rrst);
			if(!rnum.isEmpty()) rnum(item, rnum);
			if(!oprt.isEmpty()) oprt(item, oprt);
			if(!freq.isEmpty()) freq(item, freq);
			if(!mode.isEmpty()) mode(item, mode);

			return item;
		}

		/**
		 * {@link Item}に交信日時を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param time 交信日時の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void time(Item item, String time) throws Exception {
			item.add(new Time(LocalDateTime.parse(time, format)));
		}

		/**
		 * {@link Item}に相手局のコールサインを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param call コールサインの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void call(Item item, String call) throws Exception {
			item.add(fields.cache(Qxsl.CALL).field(call));
		}

		/**
		 * {@link Item}に相手局に送信したRSTQを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param srst RSTQの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void srst(Item item, String srst) throws Exception {
			item.getSent().add(fields.cache(Qxsl.RSTQ).field(srst));
		}

		/**
		 * {@link Item}に相手局に送信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param snum ナンバーの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void snum(Item item, String snum) throws Exception {
			item.getSent().add(fields.cache(Qxsl.CODE).field(snum));
		}

		/**
		 * {@link Item}に相手局から受信したRSTQを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param rrst RSTQの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rrst(Item item, String rrst) throws Exception {
			item.getRcvd().add(fields.cache(Qxsl.RSTQ).field(rrst));
		}

		/**
		 * {@link Item}に相手局から受信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param rnum ナンバーの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rnum(Item item, String rnum) throws Exception {
			item.getRcvd().add(fields.cache(Qxsl.CODE).field(rnum));
		}

		/**
		 * {@link Item}に運用者名を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param oprt 運用者名の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void oprt(Item item, String oprt) throws Exception {
			item.add(fields.cache(Qxsl.NAME).field(oprt));
		}

		/**
		 * {@link Item}に周波数帯を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param freq 周波数帯の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void freq(Item item, String freq) throws Exception {
			Integer kHz = (int) (Double.parseDouble(freq) * 1000);
			item.add(fields.cache(Qxsl.FREQ).field(kHz.toString()));
		}

		/**
		 * {@link Item}に通信方式を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param mode 通信方式の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void mode(Item item, String mode) throws Exception {
			item.add(fields.cache(Qxsl.MODE).field(mode));
		}
	}

	/**
	 * 交信記録をHLTST書式に直列化するエンコーダーです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2013/06/20
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	@Deprecated
	private final class Hl76Encoder extends TextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたストリームに出力するエンコーダーを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 * @throws IOException  SJISに対応していない場合
		 */
		public Hl76Encoder(OutputStream out) throws IOException {
			super(out, "SJIS");
			format = DateTimeFormatter.ofPattern("MM/dd HH:mm");
		}

		/**
		 * 交信記録を出力します。ストリームは閉じられます。
		 * 
		 * @param items 交信記録
		 * @throws IOException 入出力の例外
		 */
		public void write(List<Item> items) throws IOException {
			printHead();
			println();
			for(Item r : items) item(r);
			super.close();
		}

		/**
		 * 指定された{@link Item}をテキスト書式で出力します。
		 * 
		 * @param item 出力する{@link Item}
		 * @throws IOException 出力に失敗した場合
		 */
		private void item(Item item) throws IOException {
			time((Time) item.get(Qxsl.TIME));
			printSpace(1);
			printR(10, (Call) item.get(Qxsl.CALL));
			printSpace(1);
			printR(3,  (RSTQ) item.getSent().get(Qxsl.RSTQ));
			printSpace(1);
			printR(7,  (Code) item.getSent().get(Qxsl.CODE));
			printSpace(1);
			printR(3,  (RSTQ) item.getRcvd().get(Qxsl.RSTQ));
			printSpace(1);
			printR(7,  (Code) item.getRcvd().get(Qxsl.CODE));
			print("        ");
			print("1 ");
			printR(7,  (Name) item.get(Qxsl.NAME));
			printSpace(1);
			freq((Freq) item.get(Qxsl.FREQ));
			printSpace(1);
			printR(4,  (Mode) item.get(Qxsl.MODE));
			println();
		}

		/**
		 * 指定された日時を文字列として出力します。
		 * 
		 * @param date 出力する日時
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time date) throws IOException {
			if(date == null) printSpace(11);
			else print(format.format(date.value()));
		}

		/**
		 * 指定された周波数帯を文字列として出力します。
		 * 
		 * @param freq 出力する周波数帯
		 * @throws IOException 出力に失敗した場合
		 */
		private void freq(Freq freq) throws IOException {
			final String MHz = freq.toMHzString();
			printf("%-4.4s", MHz.substring(0, MHz.length() - 3));
		}
	}
}
