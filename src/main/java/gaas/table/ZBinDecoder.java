/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;

import qxsl.draft.Qxsl;
import qxsl.field.FieldManager;
import qxsl.model.Item;
import qxsl.table.TableDecoder;

import gaas.table.ZBinFactory.BandEnum;
import gaas.table.ZBinFactory.DateTime;
import gaas.table.ZBinFactory.ModeEnum;
import gaas.table.ZBinFactory.WattEnum;

/**
 * zLogバイナリデータで直列化された交信記録をデコードします。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/23
 */
public final class ZBinDecoder extends TableDecoder {
	private final DataInputStream source;
	private final FieldManager fields;
	private final ZBinFactory format;
	private DateTime tDTime;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param stream 入力
	 * @param format 書式
	 */
	public ZBinDecoder(InputStream stream, ZBinFactory format) {
		this.source = new DataInputStream(stream);
		this.fields = new FieldManager();
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
		source.readFully(new byte[0x54]);
		this.tDTime = DateTime.newInstance(source.readShort());
		source.readFully(new byte[0xAA]);
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
		time(item);
		call(item);
		sent(item);
		rcvd(item);
		source.skipBytes(1);
		sRST(item);
		rRST(item);
		source.skipBytes(4);
		mode(item);
		band(item);
		watt(item);
		mul1(item);
		mul2(item);
		source.skipBytes(3);
		name(item);
		note(item);
		source.skipBytes(14);
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
		final var buff = new byte[source.read()];
		source.readFully(buff);
		source.skipBytes(max - buff.length);
		return new String(buff, "SJIS");
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
		item.set(tDTime.decode(source.readLong()));
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
		item.set(fields.cache(Qxsl.CALL).field(read(12)));
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
	 * 交信記録に相手局まで送信したレポートを読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void sRST(Item item) throws IOException {
		final var rst = Short.reverseBytes(source.readShort());
		item.getSent().set(fields.cache(Qxsl.RSTQ).field(rst));
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
		final var rst = Short.reverseBytes(source.readShort());
		item.getRcvd().set(fields.cache(Qxsl.RSTQ).field(rst));
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
	 * 交信記録に送信電力を読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void watt(Item item) throws IOException {
		item.getSent().set(WattEnum.forIndex(source.read()).toWatt());
	}

	/**
	 * 交信記録に獲得番号を読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/10/28
	 */
	private final void mul1(Item item) throws IOException {
		item.set(fields.cache(Qxsl.MUL1).field(read(30)));
	}

	/**
	 * 交信記録に追加番号を読み取ります。
	 *
	 *
	 * @param item 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/10/28
	 */
	private final void mul2(Item item) throws IOException {
		item.set(fields.cache(Qxsl.MUL2).field(read(30)));
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
		item.set(fields.cache(Qxsl.NAME).field(read(14)));
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
		item.set(fields.cache(Qxsl.NOTE).field(read(66)));
	}
}
