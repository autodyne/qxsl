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
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import qxsl.field.*;
import qxsl.model.*;

/**
 * RTCL書式で交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/21
 *
 */
public final class RtclFormat extends TextFormat {
	/**
	 * この書式を識別する完全な名前を返します。
	 * 
	 * @return 書式の名前
	 */
	@Override
	public String getName() {
		return "rtcl";
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
		return "RTCL logsheet format";
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return new RtclDecoder(in).read();
	}

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		new RtclEncoder(out).write(items);
	}

	/**
	 * RTCL書式で直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/20
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	@Deprecated
	public final class RtclDecoder extends TextDecoder {
		private final DateTimeFormatter format;
		private final Fields fields;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public RtclDecoder(InputStream in) throws IOException {
			super(in, "JISAutoDetect");
			fields = new Fields();
			format = DateTimeFormatter.ofPattern("yyyy-MM-dd HHmm");
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
				if(!line.isEmpty() && !line.startsWith("DATE")) {
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
			String[] vals = getLine(9);
			final String time = vals[0] + " " + vals[1];
			final String band = vals[2];
			final String mode = vals[3];
			final String call = vals[4];
			final String srst = vals[5];
			final String snum = vals[6];
			final String rrst = vals[7];
			final String rnum = vals[8];

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
		 * {@link Item}に交信日時を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param time 交信日時の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void time(Item item, String time) throws Exception {
			item.set(new Time(LocalDateTime.parse(time, format)));
		}

		/**
		 * {@link Item}に周波数を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param band 周波数の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void band(Item item, String band) throws Exception {
			Integer kHz = Integer.parseInt(band);
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
	}

	/**
	 * 交信記録をRTCL書式に直列化するエンコーダーです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2013/06/20
	 * @deprecated この実装は概ね互換性がありますが、無保証です。
	 */
	@Deprecated
	public final class RtclEncoder extends TextEncoder {
		private final DateTimeFormatter format;

		/**
		 * 指定されたストリームに出力するエンコーダーを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public RtclEncoder(OutputStream out) throws IOException {
			super(out, "SJIS");
			format = DateTimeFormatter.ofPattern("yyyy-MM-dd HHmm");
		}

		/**
		 * 交信記録を出力します。ストリームは閉じられます。
		 * 
		 * @param items 交信記録
		 * @throws IOException 入出力の例外
		 */
		public void write(List<Item> items) throws IOException {
			print("DATE (JST) TIME FREQUENCY MODE CALLSIGN      ");
			println("SENTNo       RCVDNo       Mlt    Pts");
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
			print(8,  item.get(Band.class));
			print(' ');
			print(5,  item.get(Mode.class));
			print(' ');
			print(13, item.get(Call.class));
			print(' ');
			print(3,  item.getSent().get(RSTQ.class));
			print(' ');
			print(8,  item.getSent().get(Code.class));
			print(' ');
			print(3,  item.getRcvd().get(RSTQ.class));
			print(' ');
			print(8,  item.getRcvd().get(Code.class));
			println("          1");
		}

		/**
		 * 指定された日時を文字列として出力します。
		 * 
		 * @param date 出力する日時
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time date) throws IOException {
			if(date == null) printSpace(15);
			else print(format.format(date.zoned()));
		}
	}
}
