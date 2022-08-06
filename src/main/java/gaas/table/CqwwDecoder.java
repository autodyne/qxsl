/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Reader;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.model.Node;
import qxsl.table.PrintDecoder;

import gaas.table.CqwwFactory.BandEnum;

import static java.time.ZoneOffset.UTC;

/**
 * Cabrillo書式で永続化された交信記録を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/04
 */
public final class CqwwDecoder extends PrintDecoder {
	private static final int BAND = 0;
	private static final int MODE = 1;
	private static final int TIME = 2;
	private static final int SRST = 4;
	private static final int SENT = 5;
	private static final int CALL = 6;
	private static final int RRST = 7;
	private static final int RCVD = 8;
	private final DateTimeFormatter tstamp;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 */
	public CqwwDecoder(Reader reader) {
		super("cqww", reader);
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
	public final void head() throws IOException {}

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
		final var vals = split(5, 11, 14, 30, 44, 48, 55, 69, 73, 80);
		try {
			if(!vals[BAND].isEmpty()) band(item.getBoth(), vals[BAND]);
			if(!vals[MODE].isEmpty()) mode(item.getBoth(), vals[MODE]);
			if(!vals[TIME].isEmpty()) time(item.getBoth(), vals[TIME]);
			if(!vals[SRST].isEmpty()) rstq(item.getSent(), vals[SRST]);
			if(!vals[SENT].isEmpty()) code(item.getSent(), vals[SENT]);
			if(!vals[CALL].isEmpty()) call(item.getBoth(), vals[CALL]);
			if(!vals[RRST].isEmpty()) rstq(item.getRcvd(), vals[RRST]);
			if(!vals[RCVD].isEmpty()) code(item.getRcvd(), vals[RCVD]);
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
		final var format = this.tstamp.withZone(UTC);
		node.set(new Time(ZonedDateTime.parse(text, format)));
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
	 * 交信記録に周波数帯を設定します。
	 *
	 *
	 * @param node 設定する交信記録
	 * @param text 周波数帯の文字列
	 */
	private final void band(Node node, String text) {
		node.set(BandEnum.value(text.trim()).toBand());
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
}
