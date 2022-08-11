/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import qxsl.draft.*;
import qxsl.model.Item;
import qxsl.table.BasicEncoder;
import qxsl.value.Field;

import gaas.table.ZBinFactory.BandEnum;
import gaas.table.ZBinFactory.DateTime;
import gaas.table.ZBinFactory.ModeEnum;
import gaas.table.ZBinFactory.WattEnum;

/**
 * 標準構造の交信記録をzLogのZLO書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/23
 */
public final class ZBinEncoder extends BasicEncoder {
	private final DataOutputStream target;
	private final DateTime tDTime;
	private int numQSOs;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param stream 出力
	 */
	public ZBinEncoder(OutputStream stream) {
		super("zbin");
		this.target = new DataOutputStream(stream);
		this.tDTime = new DateTime();
	}

	/**
	 * タイムゾーンを返します。
	 *
	 *
	 * @return zone タイムゾーン
	 *
	 * @since 2022/06/22
	 */
	protected final short getTimeZone() {
		return (short) tDTime.getOffset();
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
	 * ストリームに交信記録の冒頭を書き込みます。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void head() throws IOException {
		target.write(new byte[0x54]);
		target.writeShort(getTimeZone());
		target.write(new byte[0xAA]);
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
		target.flush();
	}

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
		if(time == null) target.write(new byte[8]);
		else target.writeLong(tDTime.encode(time));
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
		final int s = rst == null? 599: rst.value();
		target.writeShort(Short.reverseBytes((short) s));
	}

	/**
	 * 通信方式をバイト列に変換して書き込みます。
	 *
	 *
	 * @param mode 通信方式
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void mode(Mode mode) throws IOException {
		target.writeByte(ModeEnum.valueOf(mode).ordinal());
	}

	/**
	 * 周波数帯をバイト列に変換して書き込みます。
	 *
	 *
	 * @param band 周波数帯
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void band(Band band) throws IOException {
		final var bands = BandEnum.valueOf(band);
		if(bands == null) target.writeByte(0);
		else target.writeByte(bands.ordinal());
	}

	/**
	 * 送信電力をバイト列に変換して書き込みます。
	 *
	 *
	 * @param watt 送信電力
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void watt(Watt watt) throws IOException {
		final var watts = WattEnum.valueOf(watt);
		if(watts == null) target.writeByte(0);
		else target.writeByte(watts.ordinal());
	}

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
