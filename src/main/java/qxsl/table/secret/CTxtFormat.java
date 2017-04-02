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
 * 2016年4月以前のCTESTWIN書式で交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/07/02
 *
 */
public final class CTxtFormat extends TextFormat {
	/**
	 * この書式を識別する完全な名前を返します。
	 * 
	 * @return 書式の名前
	 */
	@Override
	public String getName() {
		return "ctxt";
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
		return "CTESTWIN ver1.0";
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return new CTxtDecoder(in).read();
	}

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		new CTxtEncoder(out).write(items);
	}

	/**
	 * CTESTWIN書式で直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/07/02
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	@Deprecated
	private static final class CTxtDecoder extends TextDecoder {
		private final Fields fields;
		private final DateFormat format;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public CTxtDecoder(InputStream in) throws IOException {
			super(in, "sjis");
			fields = new Fields();
			format = new SimpleDateFormat("MM/dd HHmm");
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
				if(!line.matches("Worked\\s*[0-9]+\\s*stations|\\s*")) {
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
			final String time = subLine(5,  15);
			final String call = subLine(16, 27);
			final String band = subLine(28, 35);
			final String mode = subLine(36, 40);
			final String sent = subLine(41, 53);
			final String rcvd = subLine(54, -1);

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
		 * {@link Item}に周波数帯を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param band 周波数帯の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void band(Item item, String band) throws Exception {
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
	}

	/**
	 * 交信記録をCTESTWIN書式に直列化するエンコーダーです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2013/07/02
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	private static final class CTxtEncoder extends TextEncoder {
		private final Calendar calendar;

		/**
		 * 指定されたストリームに出力するエンコーダーを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 * @throws IOException  SJISに対応していない場合
		 */
		public CTxtEncoder(OutputStream out) throws IOException {
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
			printf("Worked %4s stations", items.size());
			println();
			println();
			int counter = 1;
			for(Item r : items) item(r, counter++);
			super.close();
		}

		/**
		 * 指定された{@link Item}をテキスト書式で出力します。
		 * 
		 * @param item 出力する{@link Item}
		 * @param num 出力する{@link Item}の番号
		 * @throws IOException 出力に失敗した場合
		 */
		private void item(Item item, int num) throws IOException {
			print(4, String.format("%4d", Integer.valueOf(num)));
			print(' ');
			time(item.get(Time.class));
			print(' ');
			print(11, item.get(Call.class));
			print(' ');
			band(item.get(Band.class));
			print(' ');
			print(4, item.get(Mode.class));
			print(' ');
			print(12, item.getSent().get(Code.class));
			print(' ');
			print(12, item.getRcvd().get(Code.class));
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
				printf("%2d/%2d %02d%02d", M, d, H, m);
			} else printSpace(10);
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
