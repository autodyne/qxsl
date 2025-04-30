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
 * JARL書式で永続化された受信記録を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/06
 */
public final class JSwlDecoder extends PrintDecoder {
	private static final int TIME = 1;
	private static final int BAND = 2;
	private static final int MODE = 3;
	private static final int CALL = 4;
	private static final int SENT = 5;
	private static final int RRST = 6;
	private static final int RCVD = 7;
	private static final int MUL1 = 8;
	private final DateTimeFormatter tstamp;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 */
	public JSwlDecoder(Reader reader) {
		super("jswl", reader);
		this.tstamp = getTimeDecoder();
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
			if(vals.length > TIME) time(item.getBoth(), vals[TIME]);
			if(vals.length > BAND) band(item.getBoth(), vals[BAND]);
			if(vals.length > MODE) mode(item.getBoth(), vals[MODE]);
			if(vals.length > CALL) call(item.getBoth(), vals[CALL]);
			if(vals.length > SENT) code(item.getSent(), vals[SENT]);
			if(vals.length > RRST) rstq(item.getRcvd(), vals[RRST]);
			if(vals.length > RCVD) code(item.getRcvd(), vals[RCVD]);
			if(vals.length > MUL1) mul1(item.getBoth(), vals[MUL1]);
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
	 * 交信記録にレポートを設定します。
	 *
	 *
	 * @param node 設定する交信記録
	 * @param text レポートの文字列
	 */
	private final void rstq(Node node, String text) {
		node.set(cache(Qxsl.RSTQ, text));
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
	 */
	private final void mul1(Node node, String text) {
		node.set(cache(Qxsl.MUL1, "-".equals(text)? null: text));
	}
}
