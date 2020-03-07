/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.net.URL;
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
 * qxslライブラリに標準的に付属するQXMLの書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/26
 *
 */
public final class QxmlFormat extends BaseFormat {
	public static final QName LIST = new QName("list");
	public static final QName ITEM = new QName("item");
	public static final QName RCVD = new QName("rcvd");
	public static final QName SENT = new QName("sent");
	private final String XSDPATH = "qxml.xsd";
	private final Schema schema;

	/**
	 * ライブラリからスキーマ定義を読み込んで書式を構築します。
	 *
	 * @throws SAXException スキーマ定義の問題
	 */
	public QxmlFormat() throws SAXException {
		super("qxml");
		SchemaFactory sf = SchemaFactory.newDefaultInstance();
		final URL url = QxmlFormat.class.getResource(XSDPATH);
		this.schema = sf.newSchema(url);
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		try {
			return new QxmlDecoder(is);
		} catch (XMLStreamException ex) {
			throw new UnsupportedOperationException(ex);
		}
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		try {
			return new QxmlEncoder(os);
		} catch (XMLStreamException ex) {
			throw new UnsupportedOperationException(ex);
		}
	}

	/**
	 *  qxml書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/22
	 *
	 */
	private final class QxmlDecoder implements TableDecoder {
		private final InputStream stream;
		private final FieldFormats fields;
		private final XMLInputFactory factor;
		private XMLEventReader reader = null;

		/**
		 * 指定されたストリームから交信記録を読み込むデコーダを構築します。
		 *
		 * @param is 交信記録を読み込むストリーム
		 * @throws XMLStreamException 通常は発生しない例外
		 */
		public QxmlDecoder(InputStream is) throws XMLStreamException {
			this.stream = is;
			this.fields = new FieldFormats();
			this.factor = XMLInputFactory.newInstance();
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException リソース解放に失敗した場合
		 */
		@Override
		public final void close() throws IOException {
			try {
				reader.close();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			} finally {
				stream.close();
			}
		}

		/**
		 * ストリームの内容を検証してから交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 * @throws IOException 構文の問題もしくは読み込みに失敗した場合
		 */
		@Override
		public final List<Item> decode() throws IOException {
			try {
				return valid();
			} catch (IOException ex) {
				throw ex;
			} catch (Exception ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ストリームの内容を検証してから交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 * @throws Exception 構文の問題もしくは読み込みに失敗した場合
		 */
		private final List<Item> valid() throws Exception {
			final byte[] arr = this.stream.readAllBytes();
			final InputStream stream = new ByteArrayInputStream(arr);
			schema.newValidator().validate(new StreamSource(stream));
			stream.reset();
			XMLEventReader raw = factor.createXMLEventReader(stream);
			reader = factor.createFilteredReader(raw, new Skipper());
			return items();
		}

		/**
		 * ストリームから交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 * @throws XMLStreamException 構文の問題もしくは読み込みに失敗した場合
		 */
		private final List<Item> items() throws XMLStreamException {
			final List<Item> items = new ArrayList<>();
			start(LIST);
			while(reader.peek().isStartElement()) items.add(item());
			close(LIST);
			return Collections.unmodifiableList(items);
		}

		/**
		 * 要素の開始イベントから1件の交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 * @throws XMLStreamException 構文の問題もしくは読み込みに失敗した場合
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
		 * @return 直後に終了すべき要素の名前
		 */
		private final QName fields(Tuple tuple, StartElement start) {
			final var attrs = start.getAttributes();
			while(attrs.hasNext()) {
				Attribute att = attrs.next();
				final var qname = att.getName();
				final var value = att.getValue();
				tuple.add(fields.cache(qname).field(value));
			}
			return start.getName();
		}

		/**
		 * 次のタグが指定された名前の要素の開始であるかを実稼働時に確認します。
		 *
		 * @param name 開始する要素の名前
		 * @return 指定された要素が見つかった場合に真
		 *
		 * @throws XMLStreamException 要素の読取り時の例外
		 */
		private boolean ahead(QName name) throws XMLStreamException {
			if(!reader.peek().isStartElement()) return false;
			return reader.peek().asStartElement().getName().equals(name);
		}

		/**
		 * 次のタグが指定された名前の要素の開始であるかをテスト時に確認します。
		 *
		 * @param name 開始する要素の名前
		 * @return 見つかった要素
		 *
		 * @throws XMLStreamException 要素の読取り時の例外
		 */
		private StartElement start(QName name) throws XMLStreamException {
			final StartElement start = reader.nextTag().asStartElement();
			if(start.getName().equals(name)) return start;
			final String msg = "expected <%s> but <%s> found";
			throw new XMLStreamException(String.format(msg, name, start));
		}

		/**
		 * 次のタグが指定された名前の要素の終了であるかをテスト時に確認します。
		 *
		 * @param name 終了する要素の名前
		 * @return 見つかった要素
		 *
		 * @throws XMLStreamException 要素の読取り時の例外
		 */
		private EndElement close(QName name) throws XMLStreamException {
			final EndElement close = reader.nextTag().asEndElement();
			if(close.getName().equals(name)) return close;
			final String msg = "expected </%s> but </%s> found";
			throw new XMLStreamException(String.format(msg, name, close));
		}

		/**
		 * 文字列を読み飛ばすフィルタです。
		 *
		 * @author 無線部開発班
		 *
		 * @since 2017/02/26
		 */
		private final class Skipper implements EventFilter {
			@Override
			public boolean accept(XMLEvent e) {
				return !e.isCharacters();
			}
		}
	}

	/**
	 * 交信記録をqxml書式で直列化するエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/02/22
	 *
	 */
	private final class QxmlEncoder implements TableEncoder {
		private final XMLStreamWriter writer;
		private final OutputStream stream;
		private final FieldFormats fields;
		private final Set<String> spaces;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 *
		 * @param os 交信記録を出力するストリーム
		 * @throws XMLStreamException 通常は発生しない例外
		 */
		public QxmlEncoder(OutputStream os) throws XMLStreamException {
			this.spaces = new HashSet<>();
			this.stream = os;
			this.fields = new FieldFormats();
			XMLOutputFactory f = XMLOutputFactory.newInstance();
			f.setProperty(IS_REPAIRING_NAMESPACES, Boolean.TRUE);
			writer = f.createXMLStreamWriter(stream);
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException リソース解放に失敗した場合
		 */
		@Override
		public final void close() throws IOException {
			try {
				writer.close();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			} finally {
				stream.close();
			}
		}

		/**
		 * ストリームに交信記録を出力します。
		 *
		 * @param items 出力する交信記録
		 * @throws IOException XMLの出力に伴う例外
		 */
		@Override
		public final void encode(List<Item> items) throws IOException {
			try {
				writer.writeStartDocument("UTF-8", "1.0");
				writer.writeCharacters("\n");
				writer.writeStartElement(LIST.getLocalPart());
				for(Item item: items) space(item);
				writer.writeCharacters("\n");
				for(Item item: items) item(item);
				writer.writeEndElement();
				writer.writeCharacters("\n");
				writer.writeEndDocument();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ストリームに名前空間の宣言を出力します。
		 *
		 * @param item 名前空間を使用する交信記録
		 * @throws XMLStreamException XMLの出力に伴う例外
		 */
		private final void space(Item item) throws XMLStreamException {
			final Rcvd rcvd = item.getRcvd();
			final Sent sent = item.getSent();
			for(Field field: item) space(field.name());
			for(Field field: sent) space(field.name());
			for(Field field: sent) space(field.name());
		}

		/**
		 * ストリームに名前空間の宣言を出力します。
		 *
		 * @param name 名前空間を使用する属性名
		 * @throws XMLStreamException XMLの出力に伴う例外
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
		 * @throws XMLStreamException XMLの出力に伴う例外
		 */
		private final void item(Item item) throws XMLStreamException {
			writer.writeStartElement(ITEM.getLocalPart());
			for(Field f: item) field(f);
			writer.writeCharacters("\n");
			rcvd(item.getRcvd());
			sent(item.getSent());
			writer.writeEndElement();
			writer.writeCharacters("\n");
			writer.flush();
		}

		/**
		 * ストリームに1件の相手局から受信した情報を出力します。
		 *
		 * @param rcvd 出力する相手局から受信した情報
		 * @throws XMLStreamException XMLの出力に伴う例外
		 */
		private final void rcvd(Rcvd rcvd) throws XMLStreamException {
			if(rcvd.iterator().hasNext()) {
				writer.writeEmptyElement(RCVD.getLocalPart());
				for(Field f: rcvd) field(f);
				writer.writeCharacters("\n");
			}
		}

		/**
		 * ストリームに1件の相手局まで送信した情報を出力します。
		 *
		 * @param sent 出力する相手局まで送信した情報
		 * @throws XMLStreamException XMLの出力に伴う例外
		 */
		private final void sent(Sent sent) throws XMLStreamException {
			if(sent.iterator().hasNext()) {
				writer.writeEmptyElement(SENT.getLocalPart());
				for(Field f: sent) field(f);
				writer.writeCharacters("\n");
			}
		}

		/**
		 * 属性を直列化してストリームに出力可能な形式に変換します。
		 *
		 * @param field 出力する属性
		 * @throws XMLStreamException XMLの出力に伴う例外
		 */
		private final void field(Field field) throws XMLStreamException {
			final QName qname = field.name();
			final String p = qname.getPrefix();
			final String l = qname.getLocalPart();
			final String u = qname.getNamespaceURI();
			writer.writeAttribute(p, u, l, fields.encode(field));
		}
	}
}
