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
import java.util.Collections;
import java.util.List;

import qxsl.field.*;
import qxsl.model.*;

/**
 * JARL書式で交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2014/06/03
 *
 */
public final class JarlFormat extends TextFormat {
	/**
	 * 書式を構築します。
	 */
	public JarlFormat() {
		super("jarl");
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return new JarlDecoder(in).read();
	}

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		new JarlEncoder(out).write(items);
	}

	/**
	 * JARL書式で直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2014/06/03
	 * @deprecated JARL書式の明文化された仕様はデコーダの挙動を厳密に定義しません。
	 */
	@Deprecated
	private static final class JarlDecoder extends TextDecoder {
		private final DateTimeFormatter format;
		private final Fields fields;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public JarlDecoder(InputStream in) throws IOException {
			super(in, "JISAutoDetect");
			fields = new Fields();
			format = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm");
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
				if(line.startsWith("DATE")) continue;
				if(line.startsWith("DATA")) continue;
				if(line.startsWith("----")) continue;
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
			final String[] vals = getLine(9);
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
		 * {@link Item}に周波数帯を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param band 周波数帯の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void band(Item item, String band) throws Exception {
			Integer kHz = (int) (Double.parseDouble(band) * 1000);
			item.set(fields.cache(QxmlFields.BAND, kHz.toString()));
		}

		/**
		 * {@link Item}に通信方式を設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param mode 通信方式の文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void mode(Item item, String mode) throws Exception {
			item.set(fields.cache(QxmlFields.MODE, mode));
		}

		/**
		 * {@link Item}に相手局のコールサインを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param call コールサインの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void call(Item item, String call) throws Exception {
			item.set(fields.cache(QxmlFields.CALL, call));
		}

		/**
		 * {@link Item}に相手局に送信したRSTQを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param srst RSTQの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void srst(Item item, String srst) throws Exception {
			item.getSent().set(fields.cache(QxmlFields.RSTQ, srst));
		}

		/**
		 * {@link Item}に相手局に送信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param snum ナンバーの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void snum(Item item, String snum) throws Exception {
			item.getSent().set(fields.cache(QxmlFields.CODE, snum));
		}

		/**
		 * {@link Item}に相手局から受信したRSTQを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param rrst RSTQの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rrst(Item item, String rrst) throws Exception {
			item.getRcvd().set(fields.cache(QxmlFields.RSTQ, rrst));
		}

		/**
		 * {@link Item}に相手局から受信したナンバーを設定します。
		 * 
		 * @param item 設定する{@link Item}
		 * @param rnum ナンバーの文字列
		 * @throws Exception 読み込みに失敗した場合
		 */
		private void rnum(Item item, String rnum) throws Exception {
			item.getRcvd().set(fields.cache(QxmlFields.CODE, rnum));
		}

	}

	/**
	 * 交信記録をJARL書式に直列化するエンコーダです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2014/06/03
	 * @deprecated JARL書式の明文化された仕様はエンコーダの挙動を厳密に定義しません。
	 */
	@Deprecated
	private static final class JarlEncoder extends TextEncoder {
		private final DateTimeFormatter format;
		
		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public JarlEncoder(OutputStream out) throws IOException {
			super(out, "SJIS");
			format = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm");
		}

		/**
		 * 交信記録を出力します。ストリームは閉じられます。
		 * 
		 * @param items 交信記録
		 * @throws IOException 入出力の例外
		 */
		public void write(List<Item> items) throws IOException {
			printHead("jarl");
			println();
			for(Item r: items) item(r);
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
			printSpace(1);
			band(item.get(Band.class));
			printSpace(1);
			printR(5,  item.get(Mode.class));
			printSpace(1);
			printR(13, item.get(Call.class));
			printSpace(1);
			printR(3,  item.getSent().get(RSTQ.class));
			printSpace(1);
			printR(7,  item.getSent().get(Code.class));
			printSpace(1);
			printR(3,  item.getRcvd().get(RSTQ.class));
			printSpace(1);
			printR(7,  item.getRcvd().get(Code.class));
			println("          1");
		}

		/**
		 * 指定された日時を文字列として出力します。
		 * 
		 * @param date 出力する日時
		 * @throws IOException 出力に失敗した場合
		 */
		private void time(Time date) throws IOException {
			if(date == null) printSpace(16);
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
