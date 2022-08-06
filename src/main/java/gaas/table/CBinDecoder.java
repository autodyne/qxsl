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

import qxsl.draft.Qxsl;
import qxsl.model.Item;
import qxsl.model.Node;
import qxsl.table.BasicDecoder;

import gaas.table.CBinFactory.BandEnum;
import gaas.table.CBinFactory.DateTime;
import gaas.table.CBinFactory.ModeEnum;

/**
 * LG8書式で永続化された交信記録を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/06/12
 */
public final class CBinDecoder extends BasicDecoder {
	private final DataInputStream source;
	private DateTime cDTime;
	private int numQSOs;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param stream 入力
	 */
	public CBinDecoder(InputStream stream) {
		super("cbin");
		this.source = new DataInputStream(stream);
		this.cDTime = new DateTime();
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
		final var num = source.readShort();
		final var mun = Short.reverseBytes(num);
		this.numQSOs = Short.toUnsignedInt(mun);
		source.skipBytes(6);
		var mno = new byte[8];
		source.readFully(mno);
		if(Arrays.equals(mno, "CQsoData".getBytes())) return;
		throw new IOException("malformed CTESTWIN sequence");
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
		call(item.getBoth());
		code(item.getSent());
		code(item.getRcvd());
		mode(item.getBoth());
		source.read();
		band(item.getBoth());
		source.skipBytes(5);
		time(item.getBoth());
		name(item.getBoth());
		source.skipBytes(2);
		note(item.getBoth());
		source.skipBytes(2);
		this.numQSOs--;
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
		return numQSOs > 0;
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
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void time(Node node) throws IOException {
		node.set(cDTime.decode(source.readLong()));
	}

	/**
	 * 交信記録に呼出符号を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void call(Node node) throws IOException {
		node.set(cache(Qxsl.CALL, read(20)));
	}

	/**
	 * 交信記録にナンバーを読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void code(Node node) throws IOException {
		node.set(cache(Qxsl.CODE, read(30)));
	}

	/**
	 * 交信記録に通信方式を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void mode(Node node) throws IOException {
		node.set(ModeEnum.forIndex(source.read()).toMode());
	}

	/**
	 * 交信記録に周波数帯を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void band(Node node) throws IOException {
		node.set(BandEnum.forIndex(source.read()).toBand());
	}

	/**
	 * 交信記録に運用者名を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void name(Node node) throws IOException {
		node.set(cache(Qxsl.NAME, read(20)));
	}

	/**
	 * 交信記録に交信の備考を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void note(Node node) throws IOException {
		node.set(cache(Qxsl.NOTE, read(50)));
	}
}
