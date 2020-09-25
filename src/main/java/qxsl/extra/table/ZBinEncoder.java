/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import qxsl.extra.field.*;
import qxsl.extra.table.ZBinFactory.BandEnum;
import qxsl.extra.table.ZBinFactory.DateTime;
import qxsl.extra.table.ZBinFactory.ModeEnum;
import qxsl.extra.table.ZBinFactory.WattEnum;
import qxsl.model.Field;
import qxsl.model.Item;
import qxsl.table.TableEncoder;

/**
 * 交信記録をzLogバイナリデータに直列化するエンコーダです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/23
 */
public final class ZBinEncoder extends TableEncoder {
	private final DataOutputStream target;
	private final ZBinFactory format;
	private final DateTime tDTime;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param stream 出力
	 * @param format 書式
	 */
	public ZBinEncoder(OutputStream stream, ZBinFactory format) {
		this.target = new DataOutputStream(stream);
		this.tDTime = new DateTime();
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
		target.writeShort(tDTime.getOffset());
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
	 * ストリームに書き込まずに交信記録を検査します。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void verify(Item item) throws IOException {
		for(var f: item) verify(f);
		for(var f: item.getRcvd()) verify(f);
		for(var f: item.getSent()) verify(f);
	}

	/**
	 * ストリームに書き込まずに属性を検査します。
	 *
	 *
	 * @param fld 属性
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2020/09/04
	 */
	private final void verify(Field fld) throws IOException {
		if(fld.name().equals(Qxsl.TIME)) return;
		if(fld.name().equals(Qxsl.CALL)) return;
		if(fld.name().equals(Qxsl.CODE)) return;
		if(fld.name().equals(Qxsl.RSTQ)) return;
		if(fld.name().equals(Qxsl.MODE)) return;
		if(fld.name().equals(Qxsl.BAND)) return;
		if(fld.name().equals(Qxsl.WATT)) return;
		if(fld.name().equals(Qxsl.NAME)) return;
		if(fld.name().equals(Qxsl.NOTE)) return;
		final var str = "field element '%s' is not supported";
		throw new IOException(String.format(str, fld.name()));
	}

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
		write(30, item.getSent().get(Qxsl.CODE));
		write(30, item.getRcvd().get(Qxsl.CODE));
		target.write(new byte[1]);
		rstq((RSTQ) item.getSent().get(Qxsl.RSTQ));
		rstq((RSTQ) item.getRcvd().get(Qxsl.RSTQ));
		target.write(new byte[4]);
		mode((Mode) item.get(Qxsl.MODE));
		band((Band) item.get(Qxsl.BAND));
		watt((Watt) item.getSent().get(Qxsl.WATT));
		target.write(new byte[65]);
		write(14, item.get(Qxsl.NAME));
		write(66, item.get(Qxsl.NOTE));
		target.write(new byte[14]);
		target.flush();
	}

	/**
	 * 指定された属性を指定された文字数まで書き込みます。
	 *
	 *
	 * @param n 最大文字数
	 * @param f 直列化する属性
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
		if(time == null) target.writeLong(0);
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
		final int s = rst == null? 599 : rst.value();
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
}
