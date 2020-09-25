/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.IOException;
import java.io.Reader;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import qxsl.extra.field.Qxsl;
import qxsl.extra.field.Time;
import qxsl.field.FieldManager;
import qxsl.model.Item;
import qxsl.table.PrintDecoder;

/**
 * zLogテキスト書式で直列化された交信記録をデコードします。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/25
 */
public final class ZDosDecoder extends PrintDecoder {
	private static final int TIME = 0;
	private static final int CALL = 1;
	private static final int SENT = 2;
	private static final int RCVD = 3;
	private static final int BAND = 5;
	private static final int MODE = 6;
	private static final int NOTE = 8;
	private static final String EMPTY = "";
	private final DateTimeFormatter tstamp;
	private final FieldManager fields;
	private final ZDosFactory format;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 * @param format 書式
	 */
	public ZDosDecoder(Reader reader, ZDosFactory format) {
		super(reader);
		this.format = format;
		this.fields = new FieldManager();
		this.tstamp = format.getTimeDecoderOld();
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
			if(line.startsWith("mon")) continue;
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
		final var vals = split(0, 13, 24, 37, 50, 57, 63, 68, 72, 157);
		try {
			if(!vals[TIME].isEmpty()) time(item, vals[TIME]);
			if(!vals[CALL].isEmpty()) call(item, vals[CALL]);
			if(!vals[SENT].isEmpty()) sent(item, vals[SENT]);
			if(!vals[RCVD].isEmpty()) rcvd(item, vals[RCVD]);
			if(!vals[BAND].isEmpty()) band(item, vals[BAND]);
			if(!vals[MODE].isEmpty()) mode(item, vals[MODE]);
			if(!vals[NOTE].isEmpty()) note(item, vals[NOTE]);
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
	 * 交信記録に相手局のコールサインを設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text コールサインの文字列
	 */
	private final void call(Item item, String text) {
		item.set(fields.cache(Qxsl.CALL).field(text));
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
	 * 交信記録に周波数帯を設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text 周波数帯の文字列
	 */
	private final void band(Item item, String text) {
		final var num = text.replaceAll("G$", "");
		final var exp = text.endsWith("G")? 1e6: 1e3;
		final var val = Double.parseDouble(num);
		final var kHz = String.valueOf(exp * val);
		item.set(fields.cache(Qxsl.BAND).field(kHz));
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
	 * 交信記録に交信の備考を設定します。
	 *
	 *
	 * @param item 設定する交信記録
	 * @param text 備考の文字列
	 */
	private final void note(Item item, String text) {
		final int bidx = text.indexOf("%%", 2);
		final var name = bidx > 0? text.substring(2, bidx): EMPTY;
		final var note = bidx > 0? text.substring(bidx + 2): text;
		item.set(fields.cache(Qxsl.NAME).field(name.trim()));
		item.set(fields.cache(Qxsl.NOTE).field(note.trim()));
	}
}
