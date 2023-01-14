/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.Reader;
import java.io.Writer;

import qxsl.table.PrintFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

/**
 * JARL制定の受信記録が従うR2.0書式の互換実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/06
 */
public final class JSwlFactory extends PrintFactory {
	/**
	 * 書式を構築します。
	 */
	public JSwlFactory() {
		super("jswl", "SJIS");
	}

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param reader 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public final TableDecoder decoder(Reader reader) {
		return new JSwlDecoder(reader);
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param writer 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public final TableEncoder encoder(Writer writer) {
		return new JSwlEncoder(writer);
	}
}
