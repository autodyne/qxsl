/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.sheet;

import java.io.*;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import javax.xml.namespace.QName;
import javax.xml.stream.*;
import javax.xml.stream.events.*;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.xml.sax.SAXException;

/**
 * JARLサマリーシートR2.0(R1.0)の書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public final class JarlFormat extends BaseFormat {
	public static final QName DOC = new QName("DOCUMENT");
	public static final QName SUM = new QName("SUMMARYSHEET");
	public static final QName LOG = new QName("LOGSHEET");
	private final String XSDPATH = "jarl.xsd";
	private final Schema schema;

	/**
	 * ライブラリからスキーマ定義を読み込んで書式を構築します。
	 *
	 * @throws SAXException スキーマ定義の問題
	 */
	public JarlFormat() throws SAXException {
		super("jarl");
		SchemaFactory sf = SchemaFactory.newDefaultInstance();
		final URL url = JarlFormat.class.getResource(XSDPATH);
		this.schema = sf.newSchema(url);
	}

	@Override
	public SheetDecoder decoder(Reader reader) {
		try {
			return new JarlDecoder(reader);
		} catch (XMLStreamException ex) {
			throw new UnsupportedOperationException(ex);
		}
	}

	@Override
	public SheetEncoder encoder(Writer writer) {
		return new JarlEncoder(writer);
	}

	/**
	 * JARLサマリーシートを開封するデコーダの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2014/11/04
	 */
	private final class JarlDecoder implements SheetDecoder {
		private static final String BF = "<(\\S+?) (\\S+?)=(\\S+?)>";
		private static final String FB = "<$1 $2=\"$3\">";
		private final BufferedReader reader;
		private final XMLInputFactory factor;
		private XMLEventReader events = null;

		/**
		 * 指定されたリーダから要約書類を読み込むデコーダを構築します。
		 *
		 * @param reader 読み込むリーダ
		 * @throws XMLStreamException 通常は発生しない例外
		 */
		public JarlDecoder(Reader reader) throws XMLStreamException {
			this.reader = new BufferedReader(reader);
			this.factor = XMLInputFactory.newInstance();
		}

		/**
		 * リーダを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		@Override
		public void close() throws IOException {
			try {
				events.close();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			} finally {
				reader.close();
			}
		}

		/**
		 * リーダの内容を検証してから要約書類を読み込みます。
		 *
		 * @return 読み込んだ要約書類
		 * @throws IOException 構文の問題もしくは読み込みに失敗した場合
		 */
		@Override
		public final Map<String, String> decode() throws IOException {
			try {
				return sheet();
			} catch (IOException ex) {
				throw ex;
			} catch (Exception ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * リーダの内容をXML文書に変換してから要約書類を読み込みます。
		 *
		 * @return 読み込んだ要約書類
		 * @throws Exception 構文の問題もしくは読み込みに失敗した場合
		 */
		private final Map<String, String> sheet() throws Exception {
			var text = reader.lines().collect(Collectors.joining("\n"));
			var form = text.replaceAll(BF, FB);
			return valid(String.format("<%1$s>%2$s</%1$s>", DOC, form));
		}

		/**
		 * 指定された文字列の内容を検証してから要約書類を読み込みます。
		 *
		 * @param sum 要約書類の文字列
		 * @return 読み込んだ要約書類
		 * @throws Exception 構文の問題もしくは読み込みに失敗した場合
		 */
		private final Map<String, String> valid(String sum) throws Exception {
			try(var stream = new StringReader(sum)) {
				schema.newValidator().validate(new StreamSource(stream));
			}
			try(var stream = new StringReader(sum)) {
				XMLEventReader raw = factor.createXMLEventReader(stream);
				events = factor.createFilteredReader(raw, new Skipper());
				return parse();
			}
		}

		/**
		 * 文書の構造を解析して要約書類を読み込みます。
		 *
		 * @return 読み込んだ要約書類
		 * @throws XMLStreamException 構文の問題もしくは読み込みに失敗した場合
		 */
		private final Map<String, String> parse() throws XMLStreamException {
			final Map<String, String> binds = new HashMap<>();
			events.nextTag().asStartElement(); // DOC
			events.nextTag().asStartElement(); // SUM
			while(events.peek().isStartElement()) {
				final var start = events.nextTag().asStartElement();
				final var value = events.nextEvent().asCharacters();
				final var close = events.nextEvent().asEndElement();
				binds.put(start.getName().getLocalPart(), value.getData());
			}
			binds.remove("SCORE"); // <SCORE BAND=*MHz>12</SCORE>
			events.nextTag().asEndElement();   // SUM
			events.nextTag().asStartElement(); // LOG
			final String log = events.getElementText(); // </LOG>
			binds.put(LOG.getLocalPart(), log.replaceAll("^\\R+|\\R+$", ""));
			events.nextTag().asEndElement();   // DOC
			return Collections.unmodifiableMap(binds);
		}

		/**
		 * 空の文字列を読み飛ばすフィルタです。
		 *
		 * @author 無線部開発班
		 *
		 * @since 2017/02/26
		 */
		private final class Skipper implements EventFilter {
			@Override
			public boolean accept(XMLEvent e) {
				return !e.isCharacters() || !e.asCharacters().isWhiteSpace();
			}
		}
	}

	/**
	 * JARLサマリーシートを作成するエンコーダの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/11
	 */
	private final class JarlEncoder implements SheetEncoder {
		private final PrintWriter writer;

		/**
		 * 指定されたライタに書き込むエンコーダを構築します。
		 *
		 * @param writer 要約書類を出力するライタ
		 */
		public JarlEncoder(Writer writer) {
			this.writer = new PrintWriter(writer, true);
		}

		/**
		 * ライタを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		@Override
		public void close() throws IOException {
			writer.close();
		}

		/**
		 * サマリーシートを出力します。
		 *
		 * @param map 属性名と属性値の集合
		 * @throws IOException 入出力の例外
		 */
		@Override
		public void encode(Map<String, String> map) throws IOException {
			final String table = map.getOrDefault(LOG.getLocalPart(), "");
			writer.printf("<%s VERSION=R2.0>%n", SUM);
			for(String key: map.keySet()) {
				if(key.equals(LOG.getLocalPart())) continue;
				writer.printf("<%1$s>%2$s</%1$s>%n", key, map.get(key));
			}
			writer.printf("</%s>%n", SUM);
			writer.printf("<%1$s>%n%2$s%n</%1$s>", LOG, table);
			writer.flush();
		}
	}
}
