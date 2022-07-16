/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
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
 * QxSLの交信記録が従うQXML書式の標準実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/26
 */
public final class QxmlFactory extends PrintFactory {
	public static final QName LIST = new QName("list");
	public static final QName ITEM = new QName("item");
	public static final QName BOTH = new QName("both");
	public static final QName RCVD = new QName("rcvd");
	public static final QName SENT = new QName("sent");
	public static final String LINE = "\n";

	/**
	 * 構文の定義を読み取って書式を構築します。
	 *
	 *
	 * @throws SAXException スキーマの例外
	 */
	public QxmlFactory() throws SAXException {
		super("qxml", "UTF-8");
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
		return new QxmlDecoder(reader);
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
		return new QxmlEncoder(writer);
	}
}
