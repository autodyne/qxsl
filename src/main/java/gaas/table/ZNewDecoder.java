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

import qxsl.model.Item;
import qxsl.table.BasicDecoder;

/**
 * zLogのZLOX書式で永続化された交信記録を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/06/22
 */
public final class ZNewDecoder extends BasicDecoder {
	private final DataInputStream source;
	private final ZBinDecoder reader;
	private int numQSOs;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param stream 入力
	 */
	public ZNewDecoder(InputStream stream) {
		super("znew");
		this.source = new DataInputStream(stream);
		this.reader = new ZBinDecoder(stream);
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
		reader.close();
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
		reader.setTimeZone(source.readShort());
		source.readFully(new byte[0xAA]);
		source.readFully(new byte[0x80]);
		this.numQSOs = Integer.reverseBytes(num);
		if(Arrays.equals(mno, "ZLOX".getBytes())) return;
		throw new IOException("malformed ZLOX sequence");
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
		final var item = reader.next();
		source.readFully(new byte[0x80]);
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
}
