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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.List;

import qxsl.field.*;
import qxsl.model.*;

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
	 * この書式を識別する完全な名前を返します。
	 * 
	 * @return 書式の名前
	 */
	@Override
	public String getName() {
		return "hl76";
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
		return "HLTST ver7.6 logsheet format";
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
	private static final class Hl76Decoder extends TextDecoder {
		private final Fields fields;
		private final DateFormat format;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public Hl76Decoder(InputStream in) throws IOException {
			super(in, "sjis");
			fields = new Fields();
			format = new SimpleDateFormat("MM/dd HH:mm");
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
			Item item = new Item();
			final String time = subLine(0,  11);
			final String call = subLine(12, 22);
			final String srst = subLine(23, 26);
			final String snum = subLine(27, 34);
			final String rrst = subLine(35, 38);
			final String rnum = subLine(39, 46);
			final String oprt = subLine(56, 63);
			final String band = subLine(64, 68);
			final String mode = subLine(69, -1);

			if(!time.isEmpty()) time(item, time);
			if(!call.isEmpty()) call(item, call);
			if(!srst.isEmpty()) srst(item, srst);
			if(!snum.isEmpty()) snum(item, snum);
			if(!rrst.isEmpty()) rrst(item, rrst);
			if(!rnum.isEmpty()) rnum(item, rnum);
			if(!oprt.isEmpty()) oprt(item, oprt);
			if(!band.isEmpty()) band(item, band);
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
			java.util.Date date = format.parse(time);
			Calendar c = Calendar.getInstance();
			final int year = c.get(Calendar.YEAR);
			c.setTime(date);
			c.set(Calendar.YEAR, year);
			item.set(new Time(c.getTime()));
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
		 * {@link Item}に相手局に送信したRSTQを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param srst RSTQの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void srst(Item item, String srst) throws Exception {
			item.getSent().set(fields.cache(RSTQ, srst));
		}

		/**
		 * {@link Item}に相手局に送信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param snum ナンバーの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void snum(Item item, String snum) throws Exception {
			item.getSent().set(fields.cache(CODE, snum));
		}

		/**
		 * {@link Item}に相手局から受信したRSTQを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param rrst RSTQの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rrst(Item item, String rrst) throws Exception {
			item.getRcvd().set(fields.cache(RSTQ, rrst));
		}

		/**
		 * {@link Item}に相手局から受信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param rnum ナンバーの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rnum(Item item, String rnum) throws Exception {
			item.getRcvd().set(fields.cache(CODE, rnum));
		}

		/**
		 * {@link Item}に運用者名を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param oprt 運用者名の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void oprt(Item item, String oprt) throws Exception {
			item.set(fields.cache(NAME, oprt));
		}

		/**
		 * {@link Item}に周波数帯を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param band 周波数帯の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void band(Item item, String band) throws Exception {
			Integer kHz = (int) (Double.parseDouble(band) * 1000);
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
	private static final class Hl76Encoder extends TextEncoder {
		private final Calendar calendar;

		/**
		 * 指定されたストリームに出力するエンコーダーを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 * @throws IOException  SJISに対応していない場合
		 */
		public Hl76Encoder(OutputStream out) throws IOException {
			super(out, "SJIS");
			calendar = GregorianCalendar.getInstance();
		}

		/**
		 * 交信記録を出力します。ストリームは閉じられます。
		 * 
		 * @param items 交信記録
		 * @throws IOException 入出力の例外
		 */
		public void write(List<Item> items) throws IOException {
			print("MM/DD HH:MM CallSign   ");
			print("Rst  Sent   Rst Rcv    ");
			print(" Multi  P  Ope    MHz  ");
			println("Mode");
			for(int i=0; i<73; i++) print('-');
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
			print(3, item.getSent().get(RSTQ.class));
			print(' ');
			print(7, item.getSent().get(Code.class));
			print(' ');
			print(3, item.getRcvd().get(RSTQ.class));
			print(' ');
			print(7, item.getRcvd().get(Code.class));
			print("        ");
			print("1 ");
			print(7, item.get(Name.class));
			print(' ');
			band(item.get(Band.class));
			print(' ');
			print(4, item.get(Mode.class));
			println();
		}

		/**
		 * 指定された日時を文字列として出力します。
		 * 
		 * @param date 出力する日時
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time date) throws IOException {
			if(date != null) {
				calendar.setTime(date.value());
				int M = calendar.get(Calendar.MONTH) + 1;
				int d = calendar.get(Calendar.DAY_OF_MONTH);
				int H = calendar.get(Calendar.HOUR_OF_DAY);
				int m = calendar.get(Calendar.MINUTE);
				printf("%02d/%02d %02d:%02d", M, d, H, m);
			} else printSpace(11);
		}

		/**
		 * 指定された周波数帯を文字列として出力します。
		 * 
		 * @param band 出力する周波数帯
		 * @throws IOException 出力に失敗した場合
		 */
		private void band(Band band) throws IOException {
			final String MHz = band.toMHzString();
			printf("%-4.4s", MHz.substring(0, MHz.length() - 3));
		}
	}
}
