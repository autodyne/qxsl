/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.OutputStream;

import qxsl.draft.Band;
import qxsl.draft.Mode;
import qxsl.draft.Watt;

import static qxsl.table.BasicFactory.FieldSet;

/**
 * 標準構造の交信記録をzLogのZLO書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/23
 */
public final class ZBinEncoder extends ZLogEncoder {
	private final FieldSet<Band> bandSet;
	private final FieldSet<Mode> modeSet;
	private final FieldSet<Watt> wattSet;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param stream 出力
	 */
	public ZBinEncoder(OutputStream stream) {
		super("zbin", stream);
		this.bandSet = ZBinFactory.getBandSet();
		this.modeSet = ZBinFactory.getModeSet();
		this.wattSet = ZBinFactory.getWattSet();
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
	 * 次の交信記録までバイト列を書き飛ばします。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2024/06/02
	 */
	@Override
	public final void skip() throws IOException {}

	/**
	 * 通信方式をバイト列に変換して書き込みます。
	 *
	 *
	 * @param mode 通信方式
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	@Override
	public final void mode(Mode mode) throws IOException {
		target.writeByte(modeSet.indexOf(mode));
	}

	/**
	 * 周波数帯をバイト列に変換して書き込みます。
	 *
	 *
	 * @param band 周波数帯
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	@Override
	public final void band(Band band) throws IOException {
		target.writeByte(bandSet.indexOf(band));
	}

	/**
	 * 送信電力をバイト列に変換して書き込みます。
	 *
	 *
	 * @param watt 送信電力
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	@Override
	public final void watt(Watt watt) throws IOException {
		target.writeByte(watt != null? wattSet.indexOf(watt): 0);
	}
}
