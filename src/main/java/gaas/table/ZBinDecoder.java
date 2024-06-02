/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import qxsl.draft.Band;
import qxsl.draft.Mode;
import qxsl.draft.Watt;
import qxsl.model.Node;

import static qxsl.table.BasicFactory.FieldSet;

/**
 * zLogのZLO書式で永続化された交信記録を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/23
 */
public final class ZBinDecoder extends ZLogDecoder {
	private final FieldSet<Band> bandSet;
	private final FieldSet<Mode> modeSet;
	private final FieldSet<Watt> wattSet;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param stream 入力
	 */
	public ZBinDecoder(InputStream stream) {
		super("zbin", stream);
		this.bandSet = ZBinFactory.getBandSet();
		this.modeSet = ZBinFactory.getModeSet();
		this.wattSet = ZBinFactory.getWattSet();
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
		var mno = new byte[4];
		source.readFully(mno);
		source.readFully(new byte[0x50]);
		setTimeZone(source.readShort());
		source.readFully(new byte[0xAA]);
		if(!Arrays.equals(mno, "ZLOX".getBytes())) return;
		else throw new IOException("TQSODataEx detected");
	}

	/**
	 * 次の交信記録までバイト列を読み飛ばします。
	 *
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2024/06/02
	 */
	@Override
	public final void skip() throws IOException {}

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
	 * 交信記録に通信方式を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	@Override
	public final void mode(Node node) throws IOException {
		node.set(modeSet.valueOf(source.read()));
	}

	/**
	 * 交信記録に周波数帯を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	@Override
	public final void band(Node node) throws IOException {
		node.set(bandSet.valueOf(source.read()));
	}

	/**
	 * 交信記録に送信電力を読み取ります。
	 *
	 *
	 * @param node 設定する交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	@Override
	public final void watt(Node node) throws IOException {
		node.set(wattSet.valueOf(source.read()));
	}
}
