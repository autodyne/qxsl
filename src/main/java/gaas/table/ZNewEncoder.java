/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import qxsl.model.Item;
import qxsl.table.TableEncoder;

/**
 * 標準構造の交信記録をzLogのZLOX書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/06/22
 */
public final class ZNewEncoder extends TableEncoder {
	private final DataOutputStream target;
	private final ZBinEncoder writer;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param stream 出力
	 */
	public ZNewEncoder(OutputStream stream) {
		this.target = new DataOutputStream(stream);
		this.writer = new ZBinEncoder(stream);
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
		writer.close();
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
		target.writeBytes("ZLOX");
		target.writeInt(Integer.reverseBytes(count()));
		target.write(new byte[0x4C]);
		target.writeShort(writer.getTimeZone());
		target.write(new byte[0xAA]);
		target.write(new byte[0x80]);
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
		writer.verify(item);
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
		writer.output(item);
		target.write(new byte[0x80]);
		target.flush();
	}
}
