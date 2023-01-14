/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.Reader;
import java.io.Writer;
import javax.xml.namespace.QName;

import org.xml.sax.SAXException;

import qxsl.table.PrintFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

/**
 * ADIFの交信記録が従うADX書式の部分的な実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/27
 */
public final class AdxsFactory extends PrintFactory {
	public static final QName LIST = new QName("RECORDS");
	public static final QName ITEM = new QName("RECORD");
	public static final QName HEAD = new QName("HEADER");
	public static final QName ROOT = new QName("ADX");
	public static final String NURI = "adif.org";
	public static final String LINE = "\n";

	/**
	 * 構文の定義を読み取って書式を構築します。
	 *
	 *
	 * @throws SAXException スキーマの例外
	 */
	public AdxsFactory() throws SAXException {
		super("adxs", "UTF-8");
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
		return new AdxsDecoder(reader);
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
		return new AdxsEncoder(writer);
	}
}
