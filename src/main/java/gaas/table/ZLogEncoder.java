/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import qxsl.draft.*;
import qxsl.model.Item;
import qxsl.table.BasicEncoder;
import qxsl.value.Field;

import gaas.table.ZLogFactory.DateTime;

/**
 * 標準構造の交信記録をzLogに永続化する共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2024/06/02
 */
public abstract class ZLogEncoder extends BasicEncoder {
	final DataOutputStream target;
	private final DateTime times;
	private int numQSOs;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param format 書式
	 * @param stream 出力
	 */
	public ZLogEncoder(String format, OutputStream stream) {
		super(format);
		this.times = new DateTime();
		this.target = new DataOutputStream(stream);
	}

	/**
	 * タイムゾーンを返します。
	 *
	 *
	 * @return zone タイムゾーン
	 *
	 * @since 2022/06/22
	 */
	public final short getTimeZone() {
		return (short) times.getOffset();
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	@Override
	public final void close() throws IOException {
		target.close();
	}

	/**
	 * ストリームに交信記録の末尾を書き込みます。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void foot() throws IOException {}

	/**
	 * ストリームの現在位置に交信記録を書き込みます。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void output(Item item) throws IOException {
		time((Time) item.get(Qxsl.TIME));
		write(12, item.get(Qxsl.CALL));
		write(30, item.getSent(Qxsl.CODE));
		write(30, item.getRcvd(Qxsl.CODE));
		target.write(new byte[1]);
		rstq((RSTQ) item.getSent(Qxsl.RSTQ));
		rstq((RSTQ) item.getRcvd(Qxsl.RSTQ));
		target.write(new byte[4]);
		mode((Mode) item.get(Qxsl.MODE));
		band((Band) item.get(Qxsl.BAND));
		watt((Watt) item.getSent(Qxsl.WATT));
		write(30, item.get(Qxsl.MUL1));
		write(30, item.get(Qxsl.MUL2));
		target.write(new byte[3]);
		write(14, item.get(Qxsl.NAME));
		write(64, item.get(Qxsl.NOTE));
		target.write(new byte[12]);
		writeID(numQSOs++);
		skip();
		target.flush();
	}

	/**
	 * 次の交信記録までバイト列を書き飛ばします。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2024/06/02
	 */
	public abstract void skip() throws IOException;

	/**
	 * 指定された属性を指定された文字数まで書き込みます。
	 *
	 *
	 * @param n 最大文字数
	 * @param f 対象の属性
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private void write(int n, Field f) throws IOException {
		final var value = f != null? f.truncate(n): "";
		final var bytes = value.getBytes("SJIS");
		target.writeByte(bytes.length);
		target.write(bytes);
		target.write(new byte[n - bytes.length]);
	}

	/**
	 * 交信日時をバイト列に変換して書き込みます。
	 *
	 *
	 * @param time 交信日時
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void time(Time time) throws IOException {
		target.writeLong(times.encode(time));
	}

	/**
	 * レポートをバイト列に変換して書き込みます。
	 *
	 *
	 * @param rst レポート
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void rstq(RSTQ rst) throws IOException {
		target.writeShort(Short.reverseBytes(rst.value().shortValue()));
	}

	/**
	 * 通信方式をバイト列に変換して書き込みます。
	 *
	 *
	 * @param mode 通信方式
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	public abstract void mode(Mode mode) throws IOException;

	/**
	 * 周波数帯をバイト列に変換して書き込みます。
	 *
	 *
	 * @param band 周波数帯
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	public abstract void band(Band band) throws IOException;

	/**
	 * 送信電力をバイト列に変換して書き込みます。
	 *
	 *
	 * @param watt 送信電力
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	public abstract void watt(Watt watt) throws IOException;

	/**
	 * 交信記録の識別のための番号を書き込みます。
	 *
	 *
	 * @param id 番号
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2022/06/26
	 */
	private final void writeID(int id) throws IOException {
		target.writeInt(Integer.reverseBytes(id * 100));
	}
}
