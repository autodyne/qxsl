/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.sheet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import javax.xml.stream.EventFilter;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import qxsl.sheet.SheetDecoder;

import static gaas.sheet.JarlFactory.DOC;

import static java.util.stream.Collectors.joining;

/**
 * JARLサマリーシートを開封するデコーダの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2014/11/04
 */
public final class JarlDecoder implements SheetDecoder {
	private final Map<String, String> values;
	private final BufferedReader source;
	private final JarlFactory format;
	private XMLEventReader reader;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 * @param format 書式
	 */
	public JarlDecoder(Reader reader, JarlFactory format) {
		this.source = new BufferedReader(reader);
		this.values = new HashMap<>();
		this.format = format;
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
	 * 指定された属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性の値
	 */
	@Override
	public final byte[] getBinary(String key) {
		return format.stringToByteArray(getString(key));
	}

	/**
	 * 指定された属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性の値
	 */
	@Override
	public final String getString(String key) {
		return values.get(key);
	}

	/**
	 * ストリームの要約書類を読み取ります。
	 *
	 *
	 * @return 要約書類を読み取ったデコーダ
	 *
	 * @throws IOException 構文上または読取り時の例外
	 */
	@Override
	public final SheetDecoder decode() throws IOException {
		try {
			this.reader = reader();
			this.read();
			return this;
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * 空の文字列を読み飛ばすフィルタです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/26
	 */
	private final class Skip implements EventFilter {
		@Override
		public boolean accept(XMLEvent e) {
			if(!e.isCharacters()) return true;
			return !e.asCharacters().isWhiteSpace();
		}
	}

	/**
	 * 文字列を読み飛ばすフィルタを適用したリーダを返します。
	 *
	 *
	 * @return リーダ
	 *
	 * @throws IOException リーダの構築に失敗した場合
	 *
	 * @since 2020/09/05
	 */
	private final XMLEventReader reader() throws IOException {
		try {
			final var string = format.valid(preprocess());
			final var factor = XMLInputFactory.newInstance();
			final var source = factor.createXMLEventReader(string);
			return factor.createFilteredReader(source, new Skip());
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームの内容を読み取って属性値を引用符で囲みます。
	 *
	 *
	 * @return 構文解析可能な文字列
	 *
	 * @throws IOException 読取り時の例外
	 */
	private final String preprocess() throws IOException {
		final var text = source.lines().collect(joining("\n"));
		final var bare = format.get("BARE");
		final var quot = format.get("QUOT");
		final var form = text.replaceAll(bare, quot);
		return String.format("<%1$s>%2$s</%1$s>", DOC, form);
	}

	/**
	 * ストリームから要約書類を読み取ります。
	 *
	 *
	 * @throws XMLStreamException 構文上または読取り時の例外
	 */
	private final void read() throws XMLStreamException {
		reader.nextTag().asStartElement();
		reader.nextTag().asStartElement();
		while(reader.peek().isStartElement()) next();
		reader.nextTag().asEndElement();
		table();
		reader.nextTag().asEndElement();
	}

	/**
	 * ストリームから次の要素を読み取ります。
	 *
	 *
	 * @throws XMLStreamException 構文上または読取り時の例外
	 */
	private final void next() throws XMLStreamException {
		final var start = reader.nextTag().asStartElement();
		final var value = value();
		final var close = reader.nextTag().asEndElement();
		values.put(start.getName().getLocalPart(), value);
	}

	/**
	 * ストリームから文字列を読み取ります。
	 *
	 *
	 * @return 読み取った文字列
	 *
	 * @throws XMLStreamException 構文上または読取り時の例外
	 */
	private final String value() throws XMLStreamException {
		if(reader.peek().isEndElement()) return "";
		return reader.nextEvent().asCharacters().getData();
	}

	/**
	 * ストリームから交信記録を読み取ります。
	 *
	 *
	 * @throws XMLStreamException 構文上または読取り時の例外
	 */
	private final void table() throws XMLStreamException {
		reader.nextTag().asStartElement();
		final var table = reader.getElementText().strip();
		this.values.put(this.format.getTableKey(), table);
	}
}
