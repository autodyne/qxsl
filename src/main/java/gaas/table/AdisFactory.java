/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.Reader;
import java.io.Writer;

import qxsl.table.PrintFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

/**
 * ADIFのうちADIと呼ばれる旧方式の書式の部分的な実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/04
 */
public final class AdisFactory extends PrintFactory {
	public static final String URI = "adif.org";
	public static final String EOH = "<eoh>";
	public static final String EOR = "<eor>";

	/**
	 * 書式を構築します。
	 */
	public AdisFactory() {
		super("adis", "ASCII");
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
		return new AdisDecoder(reader, this);
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
		return new AdisEncoder(writer, this);
	}
}
