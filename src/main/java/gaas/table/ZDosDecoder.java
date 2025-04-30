/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Reader;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.model.Node;
import qxsl.table.PrintDecoder;

/**
 * zLogのDOS書式で永続化された交信記録を解読します。
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
	private static final int MUL1 = 4;
	private static final int BAND = 5;
	private static final int MODE = 6;
	private static final int NOTE = 8;
	private static final String EMPTY = "";
	private final DateTimeFormatter tstamp;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 */
	public ZDosDecoder(Reader reader) {
		super("zdos", reader);
		this.tstamp = getTimeDecoderOld();
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
			if(!vals[TIME].isEmpty()) time(item.getBoth(), vals[TIME]);
			if(!vals[CALL].isEmpty()) call(item.getBoth(), vals[CALL]);
			if(!vals[SENT].isEmpty()) code(item.getSent(), vals[SENT]);
			if(!vals[RCVD].isEmpty()) code(item.getRcvd(), vals[RCVD]);
			if(!vals[MUL1].isEmpty()) mul1(item.getBoth(), vals[MUL1]);
			if(!vals[BAND].isEmpty()) band(item.getBoth(), vals[BAND]);
			if(!vals[MODE].isEmpty()) mode(item.getBoth(), vals[MODE]);
			if(!vals[NOTE].isEmpty()) note(item.getBoth(), vals[NOTE]);
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
	 * @param node 設定する交信記録
	 * @param text 交信日時の文字列
	 */
	private final void time(Node node, String text) {
		node.set(new Time(LocalDateTime.parse(text, tstamp)));
	}

	/**
	 * 交信記録に呼出符号を設定します。
	 *
	 *
	 * @param node 設定する交信記録
	 * @param text 呼出符号の文字列
	 */
	private final void call(Node node, String text) {
		node.set(cache(Qxsl.CALL, text));
	}

	/**
	 * 交信記録にナンバーを設定します。
	 *
	 *
	 * @param node 設定する交信記録
	 * @param text ナンバーの文字列
	 */
	private final void code(Node node, String text) {
		node.set(cache(Qxsl.CODE, text));
	}

	/**
	 * 交信記録に獲得番号を設定します。
	 *
	 *
	 * @param node 設定する交信記録
	 * @param text ナンバーの文字列
	 *
	 * @since 2020/10/28
	 */
	private final void mul1(Node node, String text) {
		node.set(cache(Qxsl.MUL1, text));
	}

	/**
	 * 交信記録に周波数帯を設定します。
	 *
	 *
	 * @param node 設定する交信記録
	 * @param text 周波数帯の文字列
	 */
	private final void band(Node node, String text) {
		text = text.replaceAll("(\\d+)$", "$1MHz");
		text = text.replaceAll("(\\d+)G", "$1GHz");
		node.set(cache(Qxsl.BAND, text));
	}

	/**
	 * 交信記録に通信方式を設定します。
	 *
	 *
	 * @param node 設定する交信記録
	 * @param text 通信方式の文字列
	 */
	private final void mode(Node node, String text) {
		node.set(cache(Qxsl.MODE, text));
	}

	/**
	 * 交信記録に交信の備考を設定します。
	 *
	 *
	 * @param node 設定する交信記録
	 * @param text 備考の文字列
	 */
	private final void note(Node node, String text) {
		final int bidx = text.indexOf("%%", 2);
		final var name = bidx > 0? text.substring(2, bidx): EMPTY;
		final var note = bidx > 0? text.substring(bidx + 2): text;
		node.set(cache(Qxsl.NAME, name.trim()));
		node.set(cache(Qxsl.NOTE, note.trim()));
	}
}
