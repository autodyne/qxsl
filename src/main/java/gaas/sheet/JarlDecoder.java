/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.sheet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;
import javax.xml.stream.EventFilter;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import qxsl.sheet.PrintDecoder;
import qxsl.sheet.SheetDecoder;

import static gaas.sheet.JarlFactory.DOC;

import static java.text.Normalizer.Form.NFKC;
import static java.text.Normalizer.normalize;
import static java.util.stream.Collectors.joining;

/**
 * JARL書式で永続化された要約書類を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2014/11/04
 */
public final class JarlDecoder extends PrintDecoder {
	private final Map<String, String> values;
	private final BufferedReader source;
	private XMLEventReader reader;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 */
	public JarlDecoder(Reader reader) {
		super("jarl", "SJIS");
		this.values = new HashMap<>();
		this.source = new BufferedReader(reader);
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
			final var reader = verify(new StringReader(preprocess()));
			final var factor = XMLInputFactory.newInstance();
			final var source = factor.createXMLEventReader(reader);
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
		final var bare = get("BARE");
		final var quot = get("QUOT");
		final var form = normal(text).replaceAll(bare, quot);
		return String.format("<%1$s>%2$s</%1$s>", DOC, form);
	}

	/**
	 * 指定された文字列を正規化します。
	 *
	 *
	 * @param text 文字列
	 *
	 * @return 正規化された文字列
	 *
	 * @since 2024/07/06
	 */
	private final String normal(String text) {
		return normalize(text, NFKC);
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
		next();
		reader.nextTag().asEndElement();
	}

	/**
	 * ストリームから次の要素を読み取ります。
	 *
	 *
	 * @throws XMLStreamException 構文上または読取り時の例外
	 */
	private final void next() throws XMLStreamException {
		final var start = reader.nextEvent().asStartElement();
		final var value = reader.getElementText().strip();
		values.put(start.getName().getLocalPart(), value);
	}
}
