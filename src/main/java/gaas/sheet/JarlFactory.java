/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.sheet;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import javax.xml.namespace.QName;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.SAXException;

import qxsl.sheet.PrintFactory;
import qxsl.sheet.SheetDecoder;
import qxsl.sheet.SheetEncoder;

/**
 * JARLサマリーシートR2.0の要約書類を表現する書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public final class JarlFactory extends PrintFactory {
	public static final QName DOC = new QName("DOCUMENT");
	public static final QName SUM = new QName("SUMMARYSHEET");
	private final String JXSD = "jarl.xsd";
	private final Schema schema;

	/**
	 * 構文の定義を読み取って書式を構築します。
	 *
	 *
	 * @throws SAXException スキーマの例外
	 */
	public JarlFactory() throws SAXException {
		super("jarl", "SJIS");
		this.schema = loadSchema();
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
		return new JarlDecoder(reader, this);
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
		return new JarlEncoder(writer, this);
	}

	/**
	 * 書式の構文の定義をリソースから読み取ります。
	 *
	 *
	 * @return スキーマ
	 *
	 * @throws SAXException スキーマの例外
	 *
	 * @since 2020/09/05
	 */
	private final Schema loadSchema() throws SAXException {
		final var fact = SchemaFactory.newDefaultInstance();
		return fact.newSchema(getClass().getResource(JXSD));
	}

	/**
	 * 指定された文字列がこの書式に従うか検証します。
	 *
	 *
	 * @param string 文字列
	 *
	 * @return 読み取った文字列を読み直す入力
	 *
	 * @throws IOException 構文または読み取り時の例外
	 */
	public Reader valid(String string) throws IOException {
		try {
			final var reader = new StringReader(string);
			final var source = new StreamSource(reader);
			this.schema.newValidator().validate(source);
			return new StringReader(string);
		} catch (SAXException ex) {
			throw new IOException(ex);
		}
	}
}
