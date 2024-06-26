/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedHashSet;
import java.util.Set;

import qxsl.draft.Band;
import qxsl.draft.Mode;
import qxsl.draft.Name;
import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.table.BasicEncoder;
import qxsl.value.Field;

import gaas.table.CBinFactory.DateTime;

import static qxsl.table.BasicFactory.FieldSet;

/**
 * 標準構造の交信記録をLG8書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/04
 */
public final class CBinEncoder extends BasicEncoder {
	private final DataOutputStream target;
	private final FieldSet<Band> bandSet;
	private final FieldSet<Mode> modeSet;
	private final DateTime chrono;
	private final Set<Name> names;
	private Item last;
	private int count;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param stream 出力
	 */
	public CBinEncoder(OutputStream stream) {
		super("cbin");
		this.names = new LinkedHashSet<Name>();
		this.chrono = new DateTime();
		this.target = new DataOutputStream(stream);
		this.bandSet = CBinFactory.getBandSet();
		this.modeSet = CBinFactory.getModeSet();
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
		target.writeShort(Short.reverseBytes((short) count()));
		target.writeShort(0xFFFF);
		target.writeShort(0x0000);
		target.writeShort(0x0800);
		target.writeBytes("CQsoData");
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
	public final void foot() throws IOException {
		confs();
		names();
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
		if(last != null) target.writeShort(0x0180);
		write(20, item.get(Qxsl.CALL));
		write(30, item.getSent(Qxsl.CODE));
		write(30, item.getRcvd(Qxsl.CODE));
		mode((Mode) item.get(Qxsl.MODE));
		target.writeByte(0);
		band((Band) item.get(Qxsl.BAND));
		target.writeByte(0);
		target.writeByte(0x0a);
		target.writeByte(0);
		target.writeByte(0);
		target.writeByte(0x80);
		time((Time) item.get(Qxsl.TIME));
		write(20, item.get(Qxsl.NAME));
		target.writeByte(0);
		target.writeByte(0);
		write(50, item.get(Qxsl.NOTE));
		names.add((Name) item.get(Qxsl.NAME));
		this.last = item;
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
		final var value = f != null? f.truncate(n - 1): "";
		final var bytes = value.getBytes("SJIS");
		target.write(bytes);
		target.write(new byte[n - bytes.length]);
	}

	/**
	 * CTESTWINの入力画面のデフォルト値を書き込みます。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void confs() throws IOException {
		if(last == null) last = new Item();
		mode((Mode) last.get(Qxsl.MODE));
		target.writeByte(0);
		target.writeInt(0);
		band((Band) last.get(Qxsl.BAND));
		target.writeByte(0);
		target.writeByte(0); // contest ID
		target.writeByte(0);
		scoresForAADX();
	}

	/**
	 * CTESTWINのAADXコンテスト用の点数設定を書き込みます。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void scoresForAADX() throws IOException {
		final int BANDS = 46; // 23 * (Asia & Non-Asia)
		target.writeShort(0x0000);   // Global Settings
		for(int i=0; i<BANDS; i++) target.writeShort(0x0100);
	}

	/**
	 * CTESTWINの運用者名のリストの設定を書き込みます。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void names() throws IOException {
		final var extracted = names.stream().limit(30);
		for(var name: extracted.toArray(Name[]::new)) write(20, name);
		target.write(new byte[Math.max(0, (30 - names.size()) * 20)]);
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
		target.writeLong(chrono.encode(time));
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
		if(mode == null) target.writeByte(0);
		else target.writeByte(modeSet.indexOf(mode));
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
		if(band == null) target.writeByte(0);
		else target.writeByte(bandSet.indexOf(band));
	}
}
