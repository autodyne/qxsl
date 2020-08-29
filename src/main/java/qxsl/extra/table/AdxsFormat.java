/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.xml.namespace.QName;
import javax.xml.stream.*;
import javax.xml.stream.events.EndElement;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.SAXException;

import qxsl.field.FieldFormats;
import qxsl.model.Field;
import qxsl.model.Item;
import qxsl.model.Tuple;

/**
 * ADIFのうちADXと呼ばれる新方式の書式の部分的な実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/27
 */
public final class AdxsFormat extends BaseFormat {
	public static final QName LIST = new QName("RECORDS");
	public static final QName ITEM = new QName("RECORD");
	public static final QName HEAD = new QName("HEADER");
	public static final QName ROOT = new QName("ADX");
	private final String NAMEURI = "adif.org";
	private final String XSDPATH = "adxs.xsd";
	private final String CHARSET = "UTF-8";
	private final String LN = "\n";
	private final Schema schema;

	/**
	 * ライブラリからスキーマ定義を読み込んで書式を構築します。
	 *
	 * @throws SAXException スキーマ定義の問題
	 */
	public AdxsFormat() throws SAXException {
		super("adxs");
		final SchemaFactory sf = SchemaFactory.newDefaultInstance();
		this.schema = sf.newSchema(getClass().getResource(XSDPATH));
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		return new AdxsDecoder(is);
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		return new AdxsEncoder(os);
	}

	/**
	 * ADXサブセット書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 */
	private final class AdxsDecoder implements TableDecoder {
		private final InputStream stream;
		private final FieldFormats fields;
		private XMLEventReader reader;
		private InputStream buf;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 *
		 * @param is 交信記録を読み込むストリーム
		 */
		public AdxsDecoder(InputStream is) {
			this.stream = is;
			this.fields = new FieldFormats();
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException 解放の例外
		 */
		@Override
		public final void close() throws IOException {
			try(stream) {
				reader.close();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ストリームの内容を検証してから交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 *
		 * @throws IOException 構文もしくは読み込みの例外
		 */
		@Override
		public final List<Item> decode() throws IOException {
			this.buf = new ByteArrayInputStream(stream.readAllBytes());
			try {
				valid();
				return items();
			} catch (XMLStreamException | SAXException ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ストリームの内容を検証します。
		 *
		 * @throws  IOException 読み込みに失敗した場合
		 * @throws SAXException 構文の例外
		 */
		private final void valid() throws IOException, SAXException {
			schema.newValidator().validate(new StreamSource(buf));
			buf.reset();
		}

		/**
		 * ストリームから交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 *
		 * @throws XMLStreamException 構文もしくは読み込みの例外
		 */
		private final List<Item> items() throws XMLStreamException {
			final var fac = XMLInputFactory.newInstance();
			final var raw = fac.createXMLEventReader(buf);
			this.reader = fac.createFilteredReader(raw, this::skip);
			final var list = new ArrayList<Item>();
			start(ROOT);
			this.head();
			start(LIST);
			while(ahead(ITEM)) list.add(item());
			close(LIST);
			close(ROOT);
			return Collections.unmodifiableList(list);
		}

		/**
		 * 指定されたイベントを無視すべきか判断します。
		 *
		 * @param e イベント
		 * @return 空白文字列の場合は偽
		 */
		private boolean skip(XMLEvent e) {
			if(!e.isCharacters()) return true;
			return !e.asCharacters().isWhiteSpace();
		}

		/**
		 * 交信記録の冒頭部分を読み込みます。
		 *
		 * @throws XMLStreamException 構文もしくは読み込みの例外
		 */
		private final void head() throws XMLStreamException {
			start(HEAD);
			while(reader.peek().isStartElement()) {
				final XMLEvent tag = reader.nextTag();
				reader.nextEvent();
				close(tag.asStartElement().getName());
			}
			close(HEAD);
		}

		/**
		 * 要素の開始イベントから1件の交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 *
		 * @throws XMLStreamException 構文もしくは読み込みの例外
		 */
		private final Item item() throws XMLStreamException {
			final Item item = new Item();
			start(ITEM);
			while(reader.peek().isStartElement()) field(item);
			close(ITEM);
			return item;
		}

		/**
		 * 次のタグの内容を指定されたタプルに属性として設定します。
		 *
		 * @param tuple 属性を設定するタプル
		 *
		 * @throws XMLStreamException 要素の読取り時の例外
		 */
		private final void field(Tuple tuple) throws XMLStreamException {
			final var tag = reader.nextTag().asStartElement().getName();
			final var key = new QName(NAMEURI, tag.getLocalPart(), getName());
			tuple.set(fields.cache(key).field(reader.nextEvent().toString()));
			close(tag);
		}

		/**
		 * 次のタグが指定された名前の要素の開始であるかを確認します。
		 *
		 * @param name 開始する要素の名前
		 *
		 * @return 指定された要素が見つかった場合に真
		 *
		 * @throws XMLStreamException 要素の読取り時の例外
		 */
		private boolean ahead(QName name) throws XMLStreamException {
			if(!reader.peek().isStartElement()) return false;
			return reader.peek().asStartElement().getName().equals(name);
		}

		/**
		 * 次のタグが指定された名前の要素の開始タグであるかを確認します。
		 *
		 * @param name 開始する要素の名前
		 *
		 * @return 見つかった要素
		 *
		 * @throws XMLStreamException 要素の読取り時の例外
		 */
		private StartElement start(QName name) throws XMLStreamException {
			final StartElement start = reader.nextTag().asStartElement();
			if(start.getName().equals(name)) return start;
			final String msg = "<%s> required but <%s> found";
			throw new XMLStreamException(String.format(msg, name, start));
		}

		/**
		 * 次のタグが指定された名前の要素の終了タグであるかを確認します。
		 *
		 * @param name 終了する要素の名前
		 *
		 * @return 見つかった要素
		 *
		 * @throws XMLStreamException 要素の読取り時の例外
		 */
		private EndElement close(QName name) throws XMLStreamException {
			final EndElement close = reader.nextTag().asEndElement();
			if(close.getName().equals(name)) return close;
			final String msg = "</%s> required but </%s> found";
			throw new XMLStreamException(String.format(msg, name, close));
		}
	}

	/**
	 * ADXサブセット書式で直列化された交信記録をエンコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 */
	private final class AdxsEncoder implements TableEncoder {
		private final OutputStream stream;
		private final FieldFormats fields;
		private XMLStreamWriter writer;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 *
		 * @param os 交信記録を出力するストリーム
		 */
		public AdxsEncoder(OutputStream os) {
			this.fields = new FieldFormats();
			this.stream = os;
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException 解放の例外
		 */
		@Override
		public final void close() throws IOException {
			try(stream) {
				writer.close();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ストリームに交信記録を出力します。
		 *
		 * @param items 出力する交信記録
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		@Override
		public final void encode(List<Item> items) throws IOException {
			final var of = XMLOutputFactory.newInstance();
			try {
				writer = of.createXMLStreamWriter(stream, CHARSET);
				writer.writeStartDocument(CHARSET, "1.0");
				writer.writeCharacters(LN);
				writer.writeStartElement(ROOT.getLocalPart());
				writer.writeCharacters(LN);
				writer.writeStartElement(HEAD.getLocalPart());
				writer.writeEndElement();
				writer.writeCharacters(LN);
				writer.writeStartElement(LIST.getLocalPart());
				writer.writeCharacters(LN);
				for(var item: items) item(item);
				writer.writeEndElement();
				writer.writeCharacters(LN);
				writer.writeEndElement();
				writer.writeCharacters(LN);
				writer.writeEndDocument();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ストリームに1件の交信記録を出力します。
		 *
		 * @param item 出力する交信記録
		 *
		 * @throws XMLStreamException 書き込みに失敗した場合
		 */
		private final void item(Item item) throws XMLStreamException {
			writer.writeStartElement(ITEM.getLocalPart());
			writer.writeCharacters(LN);
			for(var f: item) field(f);
			writer.writeEndElement();
			writer.writeCharacters(LN);
			writer.flush();
		}

		/**
		 * 属性を直列化してストリームに出力します。
		 *
		 * @param field 出力する属性
		 *
		 * @throws XMLStreamException 書き込みに失敗した場合
		 */
		private final void field(Field field) throws XMLStreamException {
			if(NAMEURI.equals(field.name().getNamespaceURI())) {
				writer.writeStartElement(field.name().getLocalPart());
				writer.writeCharacters(fields.encode(field));
				writer.writeEndElement();
				writer.writeCharacters(LN);
			}
		}
	}
}
