/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.InputStream;
import java.io.OutputStream;

import qxsl.table.BasicFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

/**
 * zLogの交信記録が従うZLOX書式の互換実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/06/22
 */
public final class ZNewFactory extends BasicFactory {
	/**
	 * 書式を構築します。
	 */
	public ZNewFactory() {
		super("znew");
	}

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param is 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public final TableDecoder decoder(InputStream is) {
		return new ZNewDecoder(is);
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param os 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public final TableEncoder encoder(OutputStream os) {
		return new ZNewEncoder(os);
	}
}
