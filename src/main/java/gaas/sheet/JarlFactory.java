/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.sheet;

import java.io.Reader;
import java.io.Writer;
import javax.xml.namespace.QName;

import org.xml.sax.SAXException;

import qxsl.sheet.PrintFactory;
import qxsl.sheet.SheetDecoder;
import qxsl.sheet.SheetEncoder;

/**
 * JARL制定の要約書類が従うR2.1書式の互換実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public final class JarlFactory extends PrintFactory {
	public static final QName DOC = new QName("DOCUMENT");
	public static final QName SUM = new QName("SUMMARYSHEET");

	/**
	 * 構文の定義を読み取って書式を構築します。
	 *
	 *
	 * @throws SAXException スキーマの例外
	 */
	public JarlFactory() throws SAXException {
		super("jarl", "SJIS");
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
		return new JarlDecoder(reader);
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
		return new JarlEncoder(writer);
	}
}
