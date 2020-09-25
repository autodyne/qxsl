/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.stream.Collectors;
import javax.xml.namespace.QName;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.SAXException;

import qxsl.table.PrintFactory;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

/**
 * ADIFと比較して名前空間による拡張性が特徴的なQXMLの書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/26
 */
public final class QxmlFactory extends PrintFactory {
	public static final QName LIST = new QName("list");
	public static final QName ITEM = new QName("item");
	public static final QName RCVD = new QName("rcvd");
	public static final QName SENT = new QName("sent");
	public static final String QXSD = "qxml.xsd";
	public static final String LINE = "\n";
	private final Schema schema;

	/**
	 * 構文の定義を読み取って書式を構築します。
	 *
	 *
	 * @throws SAXException スキーマの例外
	 */
	public QxmlFactory() throws SAXException {
		super("qxml", "UTF-8");
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
	public final TableDecoder decoder(Reader reader) {
		return new QxmlDecoder(reader, this);
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
		return new QxmlEncoder(writer, this);
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
		return fact.newSchema(getClass().getResource(QXSD));
	}

	/**
	 * 指定された入力がこの書式に従うか検証します。
	 *
	 *
	 * @param reader 入力
	 *
	 * @return 読み取った文字列を読み直す入力
	 *
	 * @throws IOException 構文または読み取り時の例外
	 */
	public Reader valid(Reader reader) throws IOException {
		try(final var br = new BufferedReader(reader)) {
			final var join = Collectors.joining("\n");
			final var text = br.lines().collect(join);
			final var read = new StringReader(text);
			final var strm = new StreamSource(read);
			this.schema.newValidator().validate(strm);
			return new StringReader(text);
		} catch (SAXException ex) {
			throw new IOException(ex);
		}
	}
}
