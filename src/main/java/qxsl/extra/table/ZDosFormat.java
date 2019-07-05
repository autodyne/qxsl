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
import java.time.ZoneId;
import java.time.format.*;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import qxsl.extra.field.*;
import qxsl.field.FieldFormats;
import qxsl.model.Item;

/**
 * zLogテキスト書式で交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/02/27
 *
 */
public final class ZDosFormat extends TextFormat {
	/**
	 * 書式を構築します。
	 */
	public ZDosFormat() {
		super("zdos", "SJIS");
	}

	@Override
	public List<Item> decode(InputStream strm, ZoneId zone) throws IOException {
		return new ZDosDecoder(strm).read();
	}

	@Override
	public void encode(OutputStream strm, List<Item> items) throws IOException {
		new ZDosEncoder(strm).write(items);
	}

	/**
	 * zLogテキスト書式で直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/25
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	@Deprecated
	private final class ZDosDecoder extends TextDecoder {
		private final DateTimeFormatter format;
		private final FieldFormats fields;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 */
		public ZDosDecoder(InputStream in) {
			super(in);
			fields = new FieldFormats();
			DateTimeFormatterBuilder fb = new DateTimeFormatterBuilder();
			fb.parseDefaulting(ChronoField.YEAR, Year.now().getValue());
			this.format = fb.appendPattern("M  ppd HHmm").toFormatter();
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
			} catch(RuntimeException ex) {
				throw new IOException(ex);
			} finally {
				super.close();
			}
		}

		private List<Item> logSheet() throws IOException {
			final List<Item> items = new ArrayList<>();
			String line;
			while((line = super.readLine()) != null) {
				if(!line.isEmpty() && !line.startsWith("mon")) {
					items.add(item(line));
				}
			}
			return Collections.unmodifiableList(items);
		}

		/**
		 * 1行の文字列から{@link Item}を1件読み込みます。
		 * 
		 * @param line 1行
		 * @return 読み込んだ{@link Item}
		 * @throws IOException 読み込みに失敗した場合
		 */
		private Item item(String line) throws IOException {
			final Item item = new Item();
			final String[] vals = split(line,
				0, 13, 24, 37, 50, 57, 63, 68, 72, -1
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
		 * {@link Item}に交信日時を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param time 交信日時の文字列
		 */
		private void time(Item item, String time) {
			item.add(new Time(LocalDateTime.parse(time, format)));
		}

		/**
		 * {@link Item}に相手局のコールサインを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param call コールサインの文字列
		 */
		private void call(Item item, String call) {
			item.add(fields.cache(Qxsl.CALL).field(call));
		}

		/**
		 * {@link Item}に相手局に送信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param sent ナンバーの文字列
		 */
		private void sent(Item item, String sent) {
			item.getSent().add(fields.cache(Qxsl.CODE).field(sent));
		}

		/**
		 * {@link Item}に相手局から受信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param rcvd ナンバーの文字列
		 */
		private void rcvd(Item item, String rcvd) {
			item.getRcvd().add(fields.cache(Qxsl.CODE).field(rcvd));
		}

		/**
		 * {@link Item}に周波数帯を設定します。
		 * 
		 * @param item 設定する{@link Item}
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
		 * {@link Item}に通信方式を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param mode 通信方式の文字列
		 */
		private void mode(Item item, String mode) {
			item.add(fields.cache(Qxsl.MODE).field(mode));
		}

		/**
		 * {@link Item}に運用者名を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param op 運用者名の文字列
		 */
		private void oprt(Item item, String op) {
			item.add(fields.cache(Qxsl.NAME).field(op));
		}

		/**
		 * {@link Item}に交信の備考を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param note 備考の文字列
		 */
		private void note(Item item, String note) {
			item.add(fields.cache(Qxsl.NOTE).field(note));
		}
	}

	/**
	 * 交信記録をzLogテキスト書式に直列化するエンコーダです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2013/02/25
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	@Deprecated
	private final class ZDosEncoder extends TextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 */
		public ZDosEncoder(OutputStream out) {
			super(out);
			format = DateTimeFormatter.ofPattern(" MM  dd HHmm");
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
			printR(12, (Code) item.getSent().get(Qxsl.CODE));
			printSpace(1);
			printR(12, (Code) item.getRcvd().get(Qxsl.CODE));
			print("        ");
			band((Band) item.get(Qxsl.BAND));
			printSpace(1);
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
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time date) throws IOException {
			if(date == null) printSpace(12);
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
				printf("%5.5s", MHz.substring(0, MHz.length() - 3));
			} else print("  10G");
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
