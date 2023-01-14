/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.sheet;

import java.io.Reader;
import java.io.Writer;

import qxsl.sheet.PrintFactory;
import qxsl.sheet.SheetDecoder;
import qxsl.sheet.SheetEncoder;

/**
 * Cabrilloの要約書類が従うV3.0書式の互換実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/03
 */
public final class Cab3Factory extends PrintFactory {
	/**
	 * 書式を構築します。
	 */
	public Cab3Factory() {
		super("cab3", "ASCII");
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
	public final SheetDecoder decoder(Reader reader) {
		return new Cab3Decoder(reader);
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
	public final SheetEncoder encoder(Writer writer) {
		return new Cab3Encoder(writer);
	}
}
