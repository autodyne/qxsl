/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Reader;
import javax.xml.namespace.QName;
import javax.xml.stream.EventFilter;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.Attribute;
import javax.xml.stream.events.EndElement;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;

import qxsl.field.FieldManager;
import qxsl.model.Item;
import qxsl.table.TableDecoder;
import qxsl.value.Tuple;

import static gaas.table.QxmlFactory.ITEM;
import static gaas.table.QxmlFactory.LIST;
import static gaas.table.QxmlFactory.RCVD;
import static gaas.table.QxmlFactory.SENT;

/**
 * QXML書式で直列化された交信記録をデコードします。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/22
 */
public final class QxmlDecoder extends TableDecoder {
	private final FieldManager fields;
	private final QxmlFactory format;
	private final Reader source;
	private XMLEventReader reader;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 * @param format 書式
	 */
	public QxmlDecoder(Reader reader, QxmlFactory format) {
		this.fields = new FieldManager();
		this.format = format;
		this.source = reader;
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	@Override
	public final void close() throws IOException {
		try {
			source.close();
			reader.close();
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームの交信記録の冒頭を読み取ります。
	 *
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void head() throws IOException {
		this.reader = reader();
		try {
			start(LIST);
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームの交信記録の末尾を読み取ります。
	 *
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void foot() throws IOException {
		try {
			close(LIST);
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームの現在位置の交信記録を読み取ります。
	 *
	 *
	 * @return 読み取った交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final Item next() throws IOException {
		try {
			return item(new Item());
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームに交信記録が存在するかを確認します。
	 *
	 *
	 * @return 交信記録を読み取れる場合は真
	 *
	 * @throws IOException 構文上または読取り時の例外
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final boolean hasNext() throws IOException {
		try {
			return ahead(ITEM);
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * 要素と属性を読み取って指定された交信記録に設定します。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @return 交信記録
	 *
	 * @throws XMLStreamException 読み取りに失敗した場合
	 *
	 * @since 2020/09/05
	 */
	private final Item item(Item item) throws XMLStreamException {
		fields(item, start(ITEM));
		if(ahead(RCVD)) close(fields(item.getRcvd(), start(RCVD)));
		if(ahead(SENT)) close(fields(item.getSent(), start(SENT)));
		close(ITEM);
		return item;
	}

	/**
	 * 文字列を読み飛ばすフィルタです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/09/05
	 */
	private static final class Skip implements EventFilter {
		@Override
		public boolean accept(XMLEvent e) {
			return !e.isCharacters();
		}
	}

	/**
	 * 文字列を読み飛ばすフィルタを適用したリーダを返します。
	 *
	 *
	 * @param filter フィルタ
	 *
	 * @return リーダ
	 *
	 * @throws IOException リーダの構築に失敗した場合
	 *
	 * @since 2020/09/05
	 */
	private final XMLEventReader reader() throws IOException {
		try {
			final var string = format.valid(this.source);
			final var factor = XMLInputFactory.newInstance();
			final var reader = factor.createXMLEventReader(string);
			return factor.createFilteredReader(reader, new Skip());
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * 次のタグが指定された名前の要素の開始であるかを確認します。
	 *
	 *
	 * @param name 開始する要素の名前
	 *
	 * @return 指定された要素が見つかった場合に真
	 *
	 * @throws XMLStreamException 要素の読み取りに失敗した場合
	 */
	private boolean ahead(QName name) throws XMLStreamException {
		if(!reader.peek().isStartElement()) return false;
		return reader.peek().asStartElement().getName().equals(name);
	}

	/**
	 * 次のタグが指定された名前の要素の開始タグであるかを確認します。
	 *
	 *
	 * @param name 開始する要素の名前
	 *
	 * @return 見つかった要素
	 *
	 * @throws XMLStreamException 要素の読み取りに失敗した場合
	 */
	private StartElement start(QName name) throws XMLStreamException {
		final var start = reader.nextTag().asStartElement();
		if(start.getName().equals(name)) return start;
		final var msg = "<%s> required but <%s> found";
		throw new XMLStreamException(String.format(msg, name, start));
	}

	/**
	 * 次のタグが指定された名前の要素の終了タグであるかを確認します。
	 *
	 *
	 * @param name 終了する要素の名前
	 *
	 * @return 見つかった要素
	 *
	 * @throws XMLStreamException 要素の読み取りに失敗した場合
	 */
	private EndElement close(QName name) throws XMLStreamException {
		final var close = reader.nextTag().asEndElement();
		if(close.getName().equals(name)) return close;
		final var msg = "</%s> required but </%s> found";
		throw new XMLStreamException(String.format(msg, name, close));
	}

	/**
	 * 要素の開始イベントから属性を読み込んで交信記録に設定します。
	 *
	 *
	 * @param tuple 属性を設定する要素
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
	 * 指定された属性値を指定された要素に設定します。
	 *
	 *
	 * @param tuple 属性を設定する要素
	 * @param field 属性
	 */
	private final void field(Tuple tuple, Attribute field) {
		tuple.set(fields.cache(field.getName()).field(field.getValue()));
	}
}
