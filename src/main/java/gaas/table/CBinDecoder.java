/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import qxsl.field.FieldManager;
import qxsl.model.Item;
import qxsl.table.TableDecoder;

import gaas.field.Qxsl;
import gaas.table.CBinFactory.BandEnum;
import gaas.table.CBinFactory.DateTime;
import gaas.table.CBinFactory.ModeEnum;

/**
 * LG8書式で直列化された交信記録をデコードします。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/06/12
 */
public final class CBinDecoder extends TableDecoder {
	private final DataInputStream source;
	private final FieldManager fields;
	private final CBinFactory format;
	private DateTime cDTime;
	private int count;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param stream 入力
	 * @param format 書式
	 */
	public CBinDecoder(InputStream stream, CBinFactory format) {
		this.source = new DataInputStream(stream);
		this.fields = new FieldManager();
		this.cDTime = new DateTime();
		this.format = format;
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
		final var QSO = "CQsoData".getBytes();
		final var hdr = source.readShort();
		final var rdh = Short.reverseBytes(hdr);
		this.count = Short.toUnsignedInt(rdh);
		source.skipBytes(6);
		var trial = new byte[8];
		source.readFully(trial);
		if(Arrays.equals(trial, QSO)) return;
		throw new IOException("unsupported");
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
		sent(item);
		rcvd(item);
		mode(item);
		source.read();
		band(item);
		source.skipBytes(5);
		time(item);
		name(item);
		source.skipBytes(2);
		note(item);
		source.skipBytes(2);
		this.count--;
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
		return count > 0;
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
		return raw.substring(0, Math.max(0, len));
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
		item.set(cDTime.decode(source.readLong()));
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
		item.set(fields.cache(Qxsl.CALL).field(read(20)));
	}

	/**
	 * 交信記録に相手局まで送信したナンバーを読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void sent(Item item) throws IOException {
		item.getSent().set(fields.cache(Qxsl.CODE).field(read(30)));
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
		item.getRcvd().set(fields.cache(Qxsl.CODE).field(read(30)));
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
		item.set(ModeEnum.forIndex(source.read()).toMode());
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
		item.set(BandEnum.forIndex(source.read()).toBand());
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
		item.set(fields.cache(Qxsl.NAME).field(read(20)));
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
		item.set(fields.cache(Qxsl.NOTE).field(read(50)));
	}
}
