/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import qxsl.draft.Qxsl;
import qxsl.model.Item;
import qxsl.table.BasicDecoder;

import gaas.table.HBinFactory.Column;
import gaas.table.HBinFactory.DateTime;

/**
 * HDB書式で永続化された交信記録を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/05
 */
public final class HBinDecoder extends BasicDecoder {
	private final Map<Column, Integer> widths;
	private final DataInputStream source;
	private final DateTime hDTime;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param stream 入力
	 */
	public HBinDecoder(InputStream stream) {
		super("hbin");
		this.source = new DataInputStream(stream);
		this.hDTime = new DateTime();
		this.widths = new HashMap<>();
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	@Override
	public final void close() throws IOException {
		source.close();
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
		source.readFully(new byte[32]);
		head(Column.CALLS);
		head(Column.IGN);
		head(Column.DATE);
		head(Column.TIME);
		head(Column.CODE);
		head(Column.GL);
		head(Column.QSL);
		head(Column.FLAG);
		head(Column.HIS);
		head(Column.MY);
		head(Column.FREQ);
		head(Column.MODE);
		head(Column.NAME);
		head(Column.QTH);
		head(Column.RMK1);
		head(Column.RMK2);
		source.skipBytes(2);
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
		call(item);
		time(item);
		rcvd(item);
		skip(Column.GL);
		skip(Column.QSL);
		skip(Column.FLAG);
		sRST(item);
		rRST(item);
		band(item);
		mode(item);
		name(item);
		skip(Column.QTH);
		note(item);
		skip(Column.RMK2);
		source.readByte();
		return item;
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
		return source.available() > 0;
	}

	/**
	 * 指定された長さまでの文字列を読み取ります。
	 *
	 *
	 * @param max 最大文字数
	 *
	 * @return 読み込んだ文字列
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final String read(int max) throws IOException {
		final var buff = new byte[max];
		source.readFully(buff);
		final var raw = new String(buff, "SJIS");
		final int len = raw.indexOf(0);
		return raw.substring(0, len > 0? len: raw.length()).trim();
	}

	/**
	 * 指定された属性の値の文字列を読み取ります。
	 *
	 *
	 * @param c 属性の名前
	 *
	 * @return 読み込んだ文字列
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final String read(Column...c) throws IOException {
		return read(Arrays.stream(c).mapToInt(widths::get).sum());
	}

	/**
	 * 指定された属性のバイト列を読み飛ばします。
	 *
	 *
	 * @param c 属性の名前
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void skip(Column c) throws IOException {
		source.readFully(new byte[widths.get(c)]);
	}

	/**
	 * 指定された属性の長さの限度を読み取ります。
	 *
	 *
	 * @param c 属性の名前
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void head(Column c) throws IOException {
		if(c.name().equals(read(16))) widths.put(c, source.read());
		else throw new IOException("malformed HAMLOG DB sequence");
		source.skipBytes(15);
	}

	/**
	 * 交信記録に交信日時を読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void time(Item item) throws IOException {
		final int dates = widths.get(Column.DATE);
		final int times = widths.get(Column.TIME);
		final var bytes = new byte[dates + times];
		source.readFully(bytes);
		item.set(hDTime.decode(bytes));
	}

	/**
	 * 交信記録に相手局のコールサインを読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void call(Item item) throws IOException {
		item.set(cache(Qxsl.CALL, read(Column.CALLS, Column.IGN)));
	}

	/**
	 * 交信記録に相手局から受信したナンバーを読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void rcvd(Item item) throws IOException {
		item.getRcvd().set(cache(Qxsl.CODE, read(Column.CODE)));
	}

	/**
	 * 交信記録に相手局まで送信したレポートを読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void sRST(Item item) throws IOException {
		item.getSent().set(cache(Qxsl.RSTQ, read(Column.HIS)));
	}

	/**
	 * 交信記録に相手局から受信したレポートを読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void rRST(Item item) throws IOException {
		item.getRcvd().set(cache(Qxsl.RSTQ, read(Column.MY)));
	}

	/**
	 * 交信記録に通信方式を読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void mode(Item item) throws IOException {
		item.set(cache(Qxsl.MODE, read(Column.MODE)));
	}

	/**
	 * 交信記録に周波数帯を読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void band(Item item) throws IOException {
		item.set(cache(Qxsl.BAND, read(Column.FREQ).concat("MHz")));
	}

	/**
	 * 交信記録に運用者名を読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void name(Item item) throws IOException {
		item.set(cache(Qxsl.NAME, read(Column.NAME)));
	}

	/**
	 * 交信記録に交信の備考を読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void note(Item item) throws IOException {
		item.set(cache(Qxsl.NOTE, read(Column.RMK1)));
	}
}
