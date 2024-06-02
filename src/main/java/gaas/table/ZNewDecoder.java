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
 * zLogのZLOX書式で永続化された交信記録を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/06/22
 */
public final class ZNewDecoder extends ZLogDecoder {
	private final FieldSet<Band> bandSet;
	private final FieldSet<Mode> modeSet;
	private final FieldSet<Watt> wattSet;
	private int numQSOs;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param stream 入力
	 */
	public ZNewDecoder(InputStream stream) {
		super("znew", stream);
		this.bandSet = ZNewFactory.getBandSet();
		this.modeSet = ZNewFactory.getModeSet();
		this.wattSet = ZNewFactory.getWattSet();
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
		final int num = source.readInt();
		source.readFully(new byte[0x4C]);
		setTimeZone(source.readShort());
		source.readFully(new byte[0xAA]);
		source.readFully(new byte[0x80]);
		this.numQSOs = Integer.reverseBytes(num);
		if(Arrays.equals(mno, "ZLOX".getBytes())) return;
		throw new IOException("malformed ZLOX sequence");
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
	public final void skip() throws IOException {
		source.readFully(new byte[0x80]);
		this.numQSOs--;
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
