/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;

import qxsl.draft.Qxsl;
import qxsl.model.Item;
import qxsl.model.Node;
import qxsl.table.BasicDecoder;

import gaas.table.ZLogFactory.DateTime;

/**
 * zLogで永続化された交信記録を解読する共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2024/06/02
 */
public abstract class ZLogDecoder extends BasicDecoder {
	final DataInputStream source;
	private DateTime times;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param format 書式
	 * @param stream 入力
	 */
	public ZLogDecoder(String format, InputStream stream) {
		super(format);
		this.source = new DataInputStream(stream);
	}

	/**
	 * タイムゾーンを設定します。
	 *
	 *
	 * @param zone タイムゾーン
	 *
	 * @since 2022/06/22
	 */
	public final void setTimeZone(short zone) {
		this.times = DateTime.newInstance(zone);
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
		time(item.getBoth());
		call(item.getBoth());
		code(item.getSent());
		code(item.getRcvd());
		source.skipBytes(1);
		rstq(item.getSent());
		rstq(item.getRcvd());
		source.skipBytes(4);
		mode(item.getBoth());
		band(item.getBoth());
		watt(item.getSent());
		mul1(item.getBoth());
		mul2(item.getBoth());
		source.skipBytes(3);
		name(item.getBoth());
		note(item.getBoth());
		source.skipBytes(4);
		source.skipBytes(4); // Pow2
		source.skipBytes(4); // Rsv2
		source.skipBytes(4); // Rsv3
		skip();
		return item;
	}

	/**
	 * 次の交信記録までバイト列を読み飛ばします。
	 *
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2024/06/02
	 */
	public abstract void skip() throws IOException;

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
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void time(Node node) throws IOException {
		node.set(times.decode(source.readLong()));
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
		node.set(cache(Qxsl.CALL, read(12)));
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
	 * 交信記録にレポートを読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	private final void rstq(Node node) throws IOException {
		node.set(cache(Qxsl.RSTQ, Short.reverseBytes(source.readShort())));
	}

	/**
	 * 交信記録に通信方式を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	public abstract void mode(Node node) throws IOException;

	/**
	 * 交信記録に周波数帯を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	public abstract void band(Node node) throws IOException;

	/**
	 * 交信記録に送信電力を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	public abstract void watt(Node node) throws IOException;

	/**
	 * 交信記録に獲得番号を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/10/28
	 */
	private final void mul1(Node node) throws IOException {
		node.set(cache(Qxsl.MUL1, read(30)));
	}

	/**
	 * 交信記録に追加番号を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/10/28
	 */
	private final void mul2(Node node) throws IOException {
		node.set(cache(Qxsl.MUL2, read(30)));
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
		node.set(cache(Qxsl.NAME, read(14)));
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
		node.set(cache(Qxsl.NOTE, read(64)));
	}
}
