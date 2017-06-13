/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table.secret;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.time.LocalDateTime;
import java.time.Year;
import java.time.format.*;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import qxsl.field.*;
import qxsl.model.*;

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
	 * この書式を識別する完全な名前を返します。
	 * 
	 * @return 書式の名前
	 */
	@Override
	public String getName() {
		return "zdos";
	}

	/**
	 * このフォーマットを適用するファイル名拡張子の不変のリストを返します。
	 * 
	 * @return ファイル名拡張子のリスト
	 */
	@Override
	public List<String> getExtensions() {
		return Collections.unmodifiableList(Arrays.asList("txt"));
	}

	/**
	 * このフォーマットの詳細をUIで表示するのに適した簡潔な文字列を返します。
	 * 
	 * @return フォーマットの説明
	 */
	@Override
	public String toString() {
		return "zLog text format (*.txt)";
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return new ZDosDecoder(in).read();
	}

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		new ZDosEncoder(out).write(items);
	}

	/**
	 * zlogテキスト書式で直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/25
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	@Deprecated
	private static final class ZDosDecoder extends TextDecoder {
		private final DateTimeFormatter format;
		private final Fields fields;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public ZDosDecoder(InputStream in) throws IOException {
			super(in, "JISAutoDetect");
			fields = new Fields();
			DateTimeFormatterBuilder fb = new DateTimeFormatterBuilder();
			fb.parseDefaulting(ChronoField.YEAR, Year.now().getValue());
			this.format = fb.appendPattern("M d HHmm").toFormatter();
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
		 * @throws Exception 読み込みに失敗した場合
		 */
		private Item item(String line) throws Exception {
			Item item = new Item();
			String mon  = subLine( 0,  3);
			String day  = subLine( 4,  7);
			String time = subLine( 8, 12);
			String call = subLine(13, 23);
			String sent = subLine(24, 36);
			String rcvd = subLine(37, 49);
			String band = subLine(57, 62);
			String mode = subLine(63, 67);
			String memo = subLine(72, -1);

			final int i = memo.indexOf("%%", 2);
			String oprt = i>0 ? memo.substring(2, i) : "";
			if(i > 0) memo = memo.substring(i + 2).trim();

			if(!time.isEmpty()) time(item, mon, day, time);
			if(!call.isEmpty()) call(item, call);
			if(!sent.isEmpty()) sent(item, sent);
			if(!rcvd.isEmpty()) rcvd(item, rcvd);
			if(!band.isEmpty()) band(item, band);
			if(!mode.isEmpty()) mode(item, mode);
			if(!oprt.isEmpty()) oprt(item, oprt);
			if(!memo.isEmpty()) memo(item, memo);

			return item;
		}

		/**
		 * {@link Item}に交信日時を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param time 交信日時の文字列の配列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void time(Item item, String... time) throws Exception {
			item.set(new Time(LocalDateTime.parse(String.join(" ", time), format)));
		}

		/**
		 * {@link Item}に相手局のコールサインを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param call コールサインの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void call(Item item, String call) throws Exception {
			item.set(fields.cache(CALL, call));
		}

		/**
		 * {@link Item}に相手局に送信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param sent ナンバーの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void sent(Item item, String sent) throws Exception {
			item.getSent().set(fields.cache(CODE, sent));
		}

		/**
		 * {@link Item}に相手局から受信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param rcvd ナンバーの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rcvd(Item item, String rcvd) throws Exception {
			item.getRcvd().set(fields.cache(CODE, rcvd));
		}

		/**
		 * {@link Item}に周波数帯を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param band 周波数帯の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void band(Item item, String band) throws Exception {
			Integer kHz;
			if(band.matches("^[0-9]+[gG]$")) {
				band = band.replaceAll("[gG]", "");
				kHz = (int) (Double.parseDouble(band) * 1000_000);
			} else {
				kHz = (int) (Double.parseDouble(band) * 1000);
			}
			item.set(fields.cache(BAND, kHz.toString()));
		}

		/**
		 * {@link Item}に通信方式を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param mode 通信方式の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void mode(Item item, String mode) throws Exception {
			item.set(fields.cache(MODE, mode));
		}

		/**
		 * {@link Item}に運用者名を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param op 運用者名の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void oprt(Item item, String op) throws Exception {
			item.set(fields.cache(NAME, op));
		}

		/**
		 * {@link Item}に交信の備考を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param memo 備考の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void memo(Item item, String memo) throws Exception {
			item.set(fields.cache(NOTE, memo));
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
	private static final class ZDosEncoder extends TextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 * @throws IOException  SJISに対応していない場合
		 */
		public ZDosEncoder(OutputStream out) throws IOException {
			super(out, "SJIS");
			format = DateTimeFormatter.ofPattern(" ppM  ppd HHmm");
		}

		/**
		 * 交信記録を出力します。ストリームは閉じられます。
		 * 
		 * @param items 交信記録
		 * @throws IOException 入出力の例外
		 */
		public void write(List<Item> items) throws IOException {
			print("mon day time  callsign      ");
			print("sent         rcvd      multi");
			print("   MHz mode pts memo");
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
			time(item.get(Time.class));
			print(' ');
			print(10, item.get(Call.class));
			print(' ');
			print(12, item.getSent().get(Code.class));
			print(' ');
			print(12, item.getRcvd().get(Code.class));
			print("        ");
			band(item.get(Band.class));
			print(' ');
			print(4, item.get(Mode.class));
			print(" 1   ");
			oprt(item.get(Name.class));
			note(item.get(Note.class));
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
			else print(format.format(date.zoned()));
		}

		/**
		 * 指定された周波数帯を文字列として出力します。
		 * 
		 * @param band 出力する周波数帯
		 * @throws IOException 出力に失敗した場合
		 */
		private void band(Band band) throws IOException {
			if(band.value() < 10_000_000) {
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
		 * @param memo 出力する備考
		 * @throws IOException 出力に失敗した場合
		 */
		private void note(Note memo) throws IOException {
			if(memo != null) print(memo.value());
		}
	}
}
