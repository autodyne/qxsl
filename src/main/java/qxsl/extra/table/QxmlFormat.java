/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.table;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.stream.*;
import javax.xml.stream.events.*;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.xml.sax.SAXException;

import qxsl.field.Qxsl;
import qxsl.model.Field;
import qxsl.model.Item;
import qxsl.model.Rcvd;
import qxsl.model.Sent;
import qxsl.model.Tuple;
import qxsl.table.Fields;

import static javax.xml.XMLConstants.*;

/**
 * qxml書式で交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/02/26
 *
 */
public final class QxmlFormat extends BaseFormat {
	public static final QName LIST = new QName("list");
	public static final QName ITEM = new QName("item");
	public static final QName RCVD = new QName("rcvd");
	public static final QName SENT = new QName("sent");
	private final Schema schema;
	
	/**
	 * {@link Schema}の定義を読み込んでフォーマットを初期化します。
	 *
	 * @throws SAXException qxml文書のスキーマに問題がある場合
	 */
	public QxmlFormat() throws SAXException {
		super("qxml");
		SchemaFactory sf = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI);
		this.schema = sf.newSchema(this.getClass().getResource("qxml.xsd"));
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return new QxmlDecoder(in).read();
	}

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		new QxmlEncoder(out).write(items);
	}

	/**
	 *  qxml書式で直列化された交信記録をデコードします。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/22
	 *
	 */
	private final class QxmlDecoder {
		private final EventReader reader;
		private final Fields fields;
		
		/**
		 * 指定したストリームから交信記録を読み込むデコーダを構築します。
		 * 
		 * @param stream 読み込むストリーム
		 * @throws IOException XMLの検証もしくは解析の準備に失敗した場合
		 */
		public QxmlDecoder(InputStream stream) throws IOException {
			InputStream rstream = new ReusableInputStream(stream);
			StreamSource source = new StreamSource(rstream);
			this.fields = new Fields();
			try {
				schema.newValidator().validate(source);
				rstream.reset();
				this.reader = new EventReader(rstream);
			} catch(SAXException | XMLStreamException ex) {
				throw new IOException(ex.getMessage(), ex);
			}
		}

		/**
		 * 交信記録を読み込みます。ストリームは閉じられます。
		 * 
		 * @return 交信記録 交信記録がなければnull
		 * @throws IOException 入出力の例外
		 */
		public List<Item> read() throws IOException {
			try {
				return items();
			} catch(IOException ex) {
				throw ex;
			} catch (Exception ex) {
				throw new IOException(ex);
			} finally {
				reader.close();
			}
		}

		/**
		 * ストリームから交信記録を読み込みます。
		 * 
		 * @return 読み込んだ交信記録
		 * @throws Exception 文法エラーまたは入出力例外
		 */
		private List<Item> items() throws Exception {
			List<Item> items = new ArrayList<>();
			reader.nextStartTag();
			StartElement se = null;
			while(reader.hasNextTag()) {
				items.add(item(reader.nextStartTag()));
			}
			return Collections.unmodifiableList(items);
		}

		/**
		 * ストリームから{@link Item}を1件読み込みます。
		 * 
		 * @param e {@link Item}の開始タグ
		 * 
		 * @return rを返す
		 * @throws Exception 文法エラーまたは入出力例外
		 */
		private Item item(StartElement e) throws Exception {
			final Item item = new Item();
			while(reader.hasNextTag()) {
				rcvd(item.getRcvd(), reader.nextStartTag());
				sent(item.getSent(), reader.nextStartTag());
			}
			reader.nextCloseTag();
			fields(item, e);
			return item;
		}	

		/**
		 * ストリームから属性を{@link Rcvd}に読み込みます。
		 * 
		 * @param rcvd 読み込み先の{@link Rcvd}
		 * @param elem {@link Rcvd}の開始タグ
		 * 
		 * @throws Exception 文法エラーまたは入出力例外
		 */
		private void rcvd(Rcvd rcvd, StartElement elem) throws Exception {
			fields(rcvd, elem);
			reader.nextCloseTag();
		}

		/**
		 * ストリームから属性を{@link Sent}に読み込みます。
		 * 
		 * @param sent 読み込み先の{@link Sent}
		 * @param elem {@link Sent}の開始タグ
		 * @throws Exception 文法エラーまたは入出力例外
		 */
		private void sent(Sent sent, StartElement elem) throws Exception {
			fields(sent, elem);
			reader.nextCloseTag();
		}

		/**
		 * ストリームから{@link Field}を生成して交信記録に設定します。
		 * 
		 * @param node タプル
		 * @param elem タプルの開始タグ
		 * @throws Exception 誤った値で生成した場合に発生する例外
		 */
		private void fields(Tuple node, StartElement elem) throws Exception {
			Iterator<?> iterator = elem.getAttributes();
			while(iterator.hasNext()) {
				Attribute att = (Attribute) iterator.next();
				node.add(fields.cache(att.getName()).field(att.getValue()));
			}
		}
	}

	/**
	 * 交信記録をqxml書式で直列化するエンコーダです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/02/22
	 *
	 */
	private final class QxmlEncoder {
		private final StreamWriter streamWriter;
		private final Fields fields;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 * 
		 * @param stream 出力先のストリーム
		 * @throws IOException XMLに書き込む準備での例外
		 */
		public QxmlEncoder(OutputStream stream) throws IOException {
			try {
				streamWriter = new StreamWriter(stream);
			} catch(XMLStreamException ex) {
				throw new IOException(ex);
			}
			fields = new Fields();
		}

		/**
		 * 交信記録を出力します。ストリームは閉じられます。
		 * 
		 * @param items 交信記録
		 * @throws IOException 入出力の例外
		 */
		public void write(List<Item> items) throws IOException {
			try {
				items(items);
			} catch (Exception ex) {
				throw new IOException(ex);
			} finally {
				streamWriter.close();
			}
		}

		/**
		 * ストリームに{@link Item}のリストを出力します。
		 *
		 * @param items 出力する交信記録
		 * @throws Exception 属性の直列化もしくはXMLの出力に伴う例外
		 */
		private void items(List<Item> items) throws Exception {
			streamWriter.writeStartDocument();
			streamWriter.writeNewLine();
			streamWriter.writeStartElement(LIST);
			streamWriter.writeNamespace(Qxsl.CALL);
			streamWriter.writeNewLine();
			for(Item item: items) item(item);
			streamWriter.writeEndElement();
			streamWriter.writeEndDocument();
		}

		/**
		 * ストリームに1件の{@link Item}を出力します。
		 *
		 * @param item 出力する{@link Item}
		 * @throws Exception 属性の直列化もしくはXMLの出力に伴う例外
		 */
		private void item(Item item) throws Exception {
			streamWriter.writeStartElement(ITEM);
			for(Field f: item) field(f);
			streamWriter.writeNewLine();
			rcvd(item.getRcvd());
			sent(item.getSent());
			streamWriter.writeEndElement();
			streamWriter.writeNewLine();
			streamWriter.flush();
		}

		/**
		 * ストリームに1件の{@link Rcvd}を出力します。
		 *
		 * @param rcvd 出力する{@link Rcvd}
		 * @throws Exception 属性の直列化もしくはXMLの出力に伴う例外
		 */
		private void rcvd(Rcvd rcvd) throws Exception {
			if(rcvd.iterator().hasNext()) {
				streamWriter.writeEmptyElement(RCVD);
				for(Field f: rcvd) field(f);
				streamWriter.writeNewLine();
				streamWriter.flush();
			}
		}

		/**
		 * ストリームに1件の{@link Sent}を出力します。
		 *
		 * @param sent 出力する{@link Sent}
		 * @throws Exception 属性の直列化もしくはXMLの出力に伴う例外
		 */
		private void sent(Sent sent) throws Exception {
			if(sent.iterator().hasNext()) {
				streamWriter.writeEmptyElement(SENT);
				for(Field f: sent) field(f);
				streamWriter.writeNewLine();
				streamWriter.flush();
			}
		}

		/**
		 * 属性値を直列化してストリームに出力します。
		 *
		 * @param field 出力する属性値
		 * @throws Exception 属性の直列化もしくはXMLの出力に伴う例外
		 */
		private void field(Field field) throws Exception {
			final QName qname = field.name();
			try {
				String f = fields.getFormat(qname).encode(field);
				streamWriter.writeAttribute(qname, f);
			} catch(NullPointerException ex) {
				throw new IOException("unknown field: " + qname);
			}
		}
	}

	/**
	 * qxml文書の入力動作に適した{@link XMLEventReader}のラッパーです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/14
	 *
	 */
	private static final class EventReader implements java.io.Closeable {
		private static final String UTF8 = "utf-8";
		private final XMLEventReader reader;
		private final InputStream stream;

		/**
		 * 指定した入力に対する{@link EventReader}を構築します
		 * 
		 * @param stream 入力
		 * @throws XMLStreamException リーダが構築できない場合
		 */
		public EventReader(InputStream stream) throws XMLStreamException {
			final XMLInputFactory fac = XMLInputFactory.newInstance();
			XMLEventReader raw = fac.createXMLEventReader(stream);
			reader = fac.createFilteredReader(raw, new Skipper());
			this.stream = stream;
		}

		/**
		 * 文字列を読み飛ばすフィルタです。
		 *
		 * @author Journal of Hamradio Informatics
		 *
		 * @since 2017/02/26
		 */
		private static final class Skipper implements EventFilter {
			@Override
			public boolean accept(XMLEvent e) {
				return !e.isCharacters();
			}
		}

		/**
		 * このリーダーを閉じて、入力元ストリームも閉じます。
		 * 
		 * @throws IOException 入出力例外が発生した場合
		 */
		public void close() throws IOException {
			try {
				reader.close();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			} finally {
				stream.close();
			}
		}

		/**
		 * 直後に開始タグを読み込んでいる場合trueを返します。
		 * 
		 * @return 次の開始タグがある場合true
		 * @throws XMLStreamException 構文エラーがあった場合
		 */
		public boolean hasNextTag() throws XMLStreamException {
			if(!reader.hasNext()) return false;
			return reader.peek().isStartElement();
		}

		/**
		 * 次の開始タグを取得します。
		 *
		 * @return 次の開始タグ
		 * @throws XMLStreamException 構文エラーがあった場合
		 */
		public StartElement nextStartTag() throws XMLStreamException {
			return reader.nextTag().asStartElement();
		}

		/**
		 * 次の終了タグを取得します。
		 *
		 * @return 次の終了タグ
		 * @throws XMLStreamException 構文エラーがあった場合
		 */
		public EndElement nextCloseTag() throws XMLStreamException {
			return reader.nextTag().asEndElement();
		}
	}

	/**
	 * qxml文書の出力動作に適した{@link XMLStreamWriter}のラッパーです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/14
	 *
	 */
	private static final class StreamWriter implements java.io.Closeable {
		private static final String UTF8 = "utf-8";
		private final XMLStreamWriter streamWriter;
		private final OutputStream outputResource;

		/**
		 * 指定した{@link OutputStream}への{@link StreamWriter}を構築します
		 * 
		 * @param stream XML文書を出力するストリーム
		 * @throws XMLStreamException {@link StreamWriter}が構築できない場合
		 */
		public StreamWriter(OutputStream stream) throws XMLStreamException {
			XMLOutputFactory of = XMLOutputFactory.newInstance();
			of.setProperty("javax.xml.stream.isRepairingNamespaces", true);
			streamWriter = of.createXMLStreamWriter(stream,UTF8);
			this.outputResource = stream;
		}

		/**
		 * このライターを閉じて、出力先ストリームも閉じます。
		 * 
		 * @throws IOException 入出力例外発生時
		 */
		public void close() throws IOException {
			try {
				streamWriter.close();
			} catch (XMLStreamException ex) {
				throw new IOException(ex);
			} finally {
				outputResource.close();
			}
		}

		/**
		 * キャッシュに格納された全てのデータをストリームに書き込みます。
		 * 
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void flush() throws XMLStreamException {
			streamWriter.flush();
		}

		/**
		 * バージョン1.0、エンコーディングUTF-8でXML宣言を書きこみます。
		 * 
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void writeStartDocument() throws XMLStreamException {
			streamWriter.writeStartDocument(UTF8, "1.0");
		}

		/**
		 * 全ての開始タグを閉じて、対応する終了タグを書き込みます。
		 * 
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void writeEndDocument() throws XMLStreamException {
			streamWriter.writeEndDocument();
		}

		/**
		 * 指定した名前を持つ開始タグをストリームに書きこみます。
		 * 
		 * @param n 要素の名前
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void writeStartElement(QName n) throws XMLStreamException {
			String p = n.getPrefix();
			String l = n.getLocalPart();
			String u = n.getNamespaceURI();
			if(!p.equals(DEFAULT_NS_PREFIX)) {
				streamWriter.writeStartElement(p, l, u);
			} else if(!u.equals(NULL_NS_URI)) {
				streamWriter.writeStartElement(u, l);
			} else {
				streamWriter.writeStartElement(l);
			}
		}

		/**
		 * 指定した名前を持つ空要素をストリームに書き込みます。
		 * 
		 * @param n 要素の名前
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void writeEmptyElement(QName n) throws XMLStreamException {
			String p = n.getPrefix();
			String l = n.getLocalPart();
			String u = n.getNamespaceURI();
			if(!p.equals(DEFAULT_NS_PREFIX)) {
				streamWriter.writeEmptyElement(p, l, u);
			} else if(!u.equals(NULL_NS_URI)) {
				streamWriter.writeEmptyElement(u, l);
			} else {
				streamWriter.writeEmptyElement(l);
			}
		}

		/**
		 * 終了タグをストリームに書き込みます。
		 * 
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void writeEndElement() throws XMLStreamException {
			streamWriter.writeEndElement();
		}

		/**
		 * 改行コードをストリームに書き込みます。
		 * 
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void writeNewLine() throws XMLStreamException {
			streamWriter.writeCharacters(System.getProperty("line.separator"));
		}

		/**
		 * 指定された名前と値の属性をストリームに書き込みます。
		 * 
		 * @param n 属性の名前
		 * @param v 属性の値
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void writeAttribute(QName n, String v) throws XMLStreamException {
			String p = n.getPrefix();
			String l = n.getLocalPart();
			String u = n.getNamespaceURI();
			if(!p.equals(DEFAULT_NS_PREFIX)) {
				streamWriter.writeAttribute(p, u, l, v);
			} else if(!u.equals(NULL_NS_URI)) {
				streamWriter.writeAttribute(u, l, v);
			} else {
				streamWriter.writeAttribute(l, v);
			}
		}

		/**
		 * 指定された名前を使用する際に必要である場合、名前空間を出力します。
		 * 
		 * @param qname 使用する名前
		 * @throws XMLStreamException 入出力例外発生時
		 */
		public void writeNamespace(QName qname) throws XMLStreamException {
			NamespaceContext space = streamWriter.getNamespaceContext();
			String prefix = qname.getPrefix();
			String uri = qname.getNamespaceURI();
			String old = space.getNamespaceURI(prefix);
			if(!uri.equals(old)) streamWriter.writeNamespace(prefix, uri);
		}
	}
}
