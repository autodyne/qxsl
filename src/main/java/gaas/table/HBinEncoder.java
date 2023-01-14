/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;

import qxsl.draft.Band;
import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.field.FieldManager.Any;
import qxsl.model.Item;
import qxsl.table.BasicEncoder;
import qxsl.value.Field;

import gaas.table.HBinFactory.Column;
import gaas.table.HBinFactory.DateTime;

/**
 * 標準構造の交信記録をHAMLOGのHDB書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/16
 */
public final class HBinEncoder extends BasicEncoder {
	private final DataOutputStream target;
	private final DateTime hDTime;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param stream 出力
	 */
	public HBinEncoder(OutputStream stream) {
		super("hbin");
		this.target = new DataOutputStream(stream);
		this.hDTime = new DateTime();
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
		target.writeInt(0x03160711);
		target.writeInt(Integer.reverseBytes(count()));
		target.writeInt(0x2102d100);
		target.write(new byte[20]);
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
		target.write(0x0d);
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
		target.write(0x1a);
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
		target.write(0x20);
		write(item.get(Qxsl.CALL), Column.CALLS, Column.IGN);
		time((Time) item.get(Qxsl.TIME));
		write(item.getRcvd(Qxsl.CODE), Column.CODE);
		write(null, Column.GL);
		write(null, Column.QSL);
		write(null, Column.FLAG);
		write(item.getSent(Qxsl.RSTQ), Column.HIS);
		write(item.getRcvd(Qxsl.RSTQ), Column.MY);
		band((Band) item.get(Qxsl.BAND));
		write(item.get(Qxsl.MODE), Column.MODE);
		write(item.get(Qxsl.NAME), Column.NAME);
		write(null, Column.QTH);
		write(item.get(Qxsl.NOTE), Column.RMK1);
		write(item.get(Qxsl.TEXT), Column.RMK2);
		target.flush();
	}

	/**
	 * 属性の値をバイト列に変換して書き込みます。
	 *
	 *
	 * @param f 属性値
	 * @param c 属性の名前
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private void write(Field f, Column...c) throws IOException {
		final int limit = Arrays.stream(c).mapToInt(Column::width).sum();
		final var chars = f != null? f.padTail(limit): " ".repeat(limit);
		target.write(chars.getBytes("SJIS"));
	}

	/**
	 * ストリームに属性の共通設定を書き込みます。
	 *
	 *
	 * @param col 属性
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void head(Column col) throws IOException {
		target.write(col.name().getBytes("ASCII"));
		target.write(new byte[11 - col.name().length()]);
		target.write(0x43);
		target.write(new byte[4]);
		target.write(col.width());
		target.write(new byte[15]);
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
		if(time == null) target.write(new byte[6]);
		else target.write(hDTime.encode(time));
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
		if(band != null) {
			final var val = band.toDecimalString(3);
			final var any = new Any(Qxsl.BAND, val);
			write(any, Column.FREQ);
		} else write(null, Column.FREQ);
	}
}
