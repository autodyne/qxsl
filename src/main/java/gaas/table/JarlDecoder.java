/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Reader;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.field.FieldManager;
import qxsl.model.Item;
import qxsl.table.PrintDecoder;

/**
 * JARL書式で直列化された交信記録をデコードします。
 *
 *
 * @author 無線部開発班
 *
 * @since 2014/06/03
 */
public final class JarlDecoder extends PrintDecoder {
	private static final int TIME = 1;
	private static final int BAND = 2;
	private static final int MODE = 3;
	private static final int CALL = 4;
	private static final int SRST = 5;
	private static final int SENT = 6;
	private static final int RRST = 7;
	private static final int RCVD = 8;
	private static final int MUL1 = 9;
	private final DateTimeFormatter tstamp;
	private final FieldManager fields;
	private final JarlFactory format;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 * @param format 書式
	 */
	public JarlDecoder(Reader reader, JarlFactory format) {
		super(reader);
		this.format = format;
		this.fields = new FieldManager();
		this.tstamp = format.getTimeDecoder();
	}

	/**
	 * ストリームの交信記録の冒頭を読み取ります。
	 *
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void head() throws IOException {
		String line;
		while((line = super.readLine()) != null) {
			if(line.isBlank()) continue;
			if(line.startsWith("DATE")) continue;
			if(line.startsWith("----")) continue;
			super.reset();
			break;
		}
	}

	/**
	 * ストリームの交信記録の末尾を読み取ります。
	 *
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void foot() throws IOException {}

	/**
	 * ストリームの現在位置の交信記録を読み取ります。
	 *
	 *
	 * @return 読み取った交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final Item next() throws IOException {
		final var item = new Item();
		try {
			final var vals = readLine().split("\\s+");
			vals[TIME] = vals[0].concat(" ").concat(vals[1]);
			if(vals.length > TIME) time(item, vals[TIME]);
			if(vals.length > BAND) band(item, vals[BAND]);
			if(vals.length > MODE) mode(item, vals[MODE]);
			if(vals.length > CALL) call(item, vals[CALL]);
			if(vals.length > SRST) sRST(item, vals[SRST]);
			if(vals.length > SENT) sent(item, vals[SENT]);
			if(vals.length > RRST) rRST(item, vals[RRST]);
			if(vals.length > RCVD) rcvd(item, vals[RCVD]);
			if(vals.length > MUL1) mul1(item, vals[MUL1]);
			return item;
		} catch (RuntimeException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームに交信記録が存在するかを確認します。
	 *
	 *
	 * @return 交信記録を読み取れる場合は真
	 *
	 * @throws IOException 構文上または読取り時の例外
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final boolean hasNext() throws IOException {
		final var exists = super.readLine() != null;
		super.reset();
		return exists;
	}

	/**
	 * 交信記録に交信日時を設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text 交信日時の文字列
	 */
	private final void time(Item item, String text) {
		item.set(new Time(LocalDateTime.parse(text, tstamp)));
	}

	/**
	 * 交信記録に周波数帯を設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text 周波数帯の文字列
	 */
	private final void band(Item item, String text) {
		final double kHz = Double.parseDouble(text) * 1000;
		item.set(fields.cache(Qxsl.BAND).field((int) kHz));
	}

	/**
	 * 交信記録に通信方式を設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text 通信方式の文字列
	 */
	private final void mode(Item item, String text) {
		item.set(fields.cache(Qxsl.MODE).field(text));
	}

	/**
	 * 交信記録に相手局の呼出符号を設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text 呼出符号の文字列
	 */
	private final void call(Item item, String text) {
		item.set(fields.cache(Qxsl.CALL).field(text));
	}

	/**
	 * 交信記録に相手局まで送信したレポートを設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text レポートの文字列
	 */
	private final void sRST(Item item, String text) {
		item.getSent().set(fields.cache(Qxsl.RSTQ).field(text));
	}

	/**
	 * 交信記録に相手局まで送信したナンバーを設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text ナンバーの文字列
	 */
	private final void sent(Item item, String text) {
		item.getSent().set(fields.cache(Qxsl.CODE).field(text));
	}

	/**
	 * 交信記録に相手局から受信したレポートを設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text レポートの文字列
	 */
	private final void rRST(Item item, String text) {
		item.getRcvd().set(fields.cache(Qxsl.RSTQ).field(text));
	}

	/**
	 * 交信記録に相手局から受信したナンバーを設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text ナンバーの文字列
	 */
	private final void rcvd(Item item, String text) {
		item.getRcvd().set(fields.cache(Qxsl.CODE).field(text));
	}

	/**
	 * 交信記録に獲得番号を設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text ナンバーの文字列
	 */
	private final void mul1(Item item, String text) {
		item.set(fields.cache(Qxsl.MUL1).field(text));
	}
}
