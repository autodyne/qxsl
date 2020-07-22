/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.xml.namespace.QName;
import javax.xml.stream.*;
import javax.xml.stream.events.*;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.xml.sax.SAXException;

import qxsl.field.FieldFormats;
import qxsl.model.Field;
import qxsl.model.Item;
import qxsl.model.Rcvd;
import qxsl.model.Sent;
import qxsl.model.Tuple;

import static javax.xml.stream.XMLOutputFactory.IS_REPAIRING_NAMESPACES;

/**
 * ADIFと比較して名前空間による拡張性が特徴的なQXMLの書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/26
 */
public final class QxmlFormat extends BaseFormat {
	public static final QName LIST = new QName("list");
	public static final QName ITEM = new QName("item");
	public static final QName RCVD = new QName("rcvd");
	public static final QName SENT = new QName("sent");
	private final String XSDPATH = "qxml.xsd";
	private final String CHARSET = "UTF-8";
	private final String LN = "\n";
	private final Schema schema;

	/**
	 * ライブラリからスキーマ定義を読み込んで書式を構築します。
	 *
	 * @throws SAXException スキーマ定義の問題
	 */
	public QxmlFormat() throws SAXException {
		super("qxml");
		final SchemaFactory sf = SchemaFactory.newDefaultInstance();
		this.schema = sf.newSchema(getClass().getResource(XSDPATH));
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		return new QxmlDecoder(is);
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		return new QxmlEncoder(os);
	}

	/**
	 * QXML書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/22
	 */
	private final class QxmlDecoder implements TableDecoder {
		private final InputStream stream;
		private final FieldFormats fields;
		private XMLEventReader reader;
		private InputStream buf;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 *
		 * @param is 交信記録を読み込むストリーム
		 */
		public QxmlDecoder(InputStream is) {
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
		 * ストリームの内容を検証してから交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
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
			start(LIST);
			while(ahead(ITEM)) list.add(item());
			close(LIST);
			return Collections.unmodifiableList(list);
		}

		/**
		 * 指定されたイベントを無視すべきか判断します。
		 *
		 * @param e イベント
		 * @return 文字列の場合は偽
		 */
		private boolean skip(XMLEvent e) {
			return !e.isCharacters();
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
			fields(item, start(ITEM));
			if(ahead(RCVD)) close(fields(item.getRcvd(), start(RCVD)));
			if(ahead(SENT)) close(fields(item.getSent(), start(SENT)));
			close(ITEM);
			return item;
		}

		/**
		 * 要素の開始イベントから属性を読み込んで交信記録に設定します。
		 *
		 * @param tuple 属性を設定するタプル
		 * @param start 開始する要素
		 *
		 * @return 直後に終了すべき要素の名前
		 */
		private final QName fields(Tuple tuple, StartElement start) {
			final var attrs = start.getAttributes();
			while(attrs.hasNext()) field(tuple, attrs.next());
			return start.getName();
		}

		/**
		 * 指定された属性値を指定されたタプルに設定します。
		 *
		 * @param tuple 属性を設定するタプル
		 * @param field 属性
		 */
		private final void field(Tuple tuple, Attribute field) {
			tuple.add(fields.cache(field.getName()).field(field.getValue()));
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
	 * 交信記録をQXML書式で直列化するエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/22
	 */
	private final class QxmlEncoder implements TableEncoder {
		private final OutputStream stream;
		private final FieldFormats fields;
		private final Set<String> spaces;
		private XMLStreamWriter writer;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 *
		 * @param os 交信記録を出力するストリーム
		 */
		public QxmlEncoder(OutputStream os) {
			this.fields = new FieldFormats();
			this.spaces = new HashSet<>();
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
		 * @throws IOException 書き出しに失敗した場合
		 */
		@Override
		public final void encode(List<Item> items) throws IOException {
			final var of = XMLOutputFactory.newInstance();
			of.setProperty(IS_REPAIRING_NAMESPACES, true);
			try {
				writer = of.createXMLStreamWriter(stream, CHARSET);
				writer.writeStartDocument(CHARSET, "1.0");
				writer.writeCharacters(LN);
				writer.writeStartElement(LIST.getLocalPart());
				for(var item: items) space(item);
				writer.writeCharacters(LN);
				for(var item: items) item(item);
				writer.writeEndElement();
				writer.writeCharacters(LN);
				writer.writeEndDocument();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ストリームに名前空間の宣言を出力します。
		 *
		 * @param item 名前空間を使用する交信記録
		 *
		 * @throws XMLStreamException 書き出しに失敗した場合
		 */
		private final void space(Item item) throws XMLStreamException {
			final var rcvd = item.getRcvd();
			final var sent = item.getSent();
			for(var field: item) space(field.name());
			for(var field: sent) space(field.name());
			for(var field: sent) space(field.name());
		}

		/**
		 * ストリームに名前空間の宣言を出力します。
		 *
		 * @param name 名前空間を使用する属性名
		 *
		 * @throws XMLStreamException 書き出しに失敗した場合
		 */
		private final void space(QName name) throws XMLStreamException {
			final String p = name.getPrefix();
			final String s = name.getNamespaceURI();
			if(!p.isEmpty() && spaces.add(p)) writer.writeNamespace(p, s);
		}

		/**
		 * ストリームに1件の交信記録を出力します。
		 *
		 * @param item 出力する交信記録
		 *
		 * @throws XMLStreamException 書き出しに失敗した場合
		 */
		private final void item(Item item) throws XMLStreamException {
			writer.writeStartElement(ITEM.getLocalPart());
			for(var f: item) field(f);
			writer.writeCharacters(LN);
			rcvd(item.getRcvd());
			sent(item.getSent());
			writer.writeEndElement();
			writer.writeCharacters(LN);
			writer.flush();
		}

		/**
		 * ストリームに1件の相手局から受信した情報を出力します。
		 *
		 * @param rcvd 出力する相手局から受信した情報
		 *
		 * @throws XMLStreamException 書き出しに失敗した場合
		 */
		private final void rcvd(Rcvd rcvd) throws XMLStreamException {
			if(rcvd.iterator().hasNext()) {
				writer.writeEmptyElement(RCVD.getLocalPart());
				for(var f: rcvd) field(f);
				writer.writeCharacters(LN);
			}
		}

		/**
		 * ストリームに1件の相手局まで送信した情報を出力します。
		 *
		 * @param sent 出力する相手局まで送信した情報
		 *
		 * @throws XMLStreamException 書き出しに失敗した場合
		 */
		private final void sent(Sent sent) throws XMLStreamException {
			if(sent.iterator().hasNext()) {
				writer.writeEmptyElement(SENT.getLocalPart());
				for(var f: sent) field(f);
				writer.writeCharacters(LN);
			}
		}

		/**
		 * 属性を直列化してストリームに出力します。
		 *
		 * @param field 出力する属性
		 *
		 * @throws XMLStreamException 書き出しに失敗した場合
		 */
		private final void field(Field field) throws XMLStreamException {
			final var p = field.name().getPrefix();
			final var l = field.name().getLocalPart();
			final var u = field.name().getNamespaceURI();
			writer.writeAttribute(p, u, l, fields.encode(field));
		}
	}
}
