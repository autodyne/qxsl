/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Writer;
import java.util.HashSet;
import java.util.Set;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import qxsl.field.FieldManager;
import qxsl.model.Item;
import qxsl.model.Rcvd;
import qxsl.model.Sent;
import qxsl.table.TableEncoder;
import qxsl.value.Field;

import static gaas.table.QxmlFactory.*;

import static javax.xml.stream.XMLOutputFactory.IS_REPAIRING_NAMESPACES;

/**
 * 交信記録をQXML書式に直列化するエンコーダです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/22
 */
public final class QxmlEncoder extends TableEncoder {
	private final FieldManager fields;
	private final Set<String> spaces;
	private final QxmlFactory format;
	private final Writer target;
	private XMLStreamWriter writer;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 * @param format 書式
	 */
	public QxmlEncoder(Writer writer, QxmlFactory format) {
		this.fields = new FieldManager();
		this.spaces = new HashSet<>();
		this.format = format;
		this.target = writer;
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
			target.close();
			writer.close();
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームに交信記録の冒頭を書き込みます。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void head() throws IOException {
		final var lp = LIST.getLocalPart();
		try {
			this.writer = writer();
			this.writer.writeStartDocument();
			this.writer.writeCharacters(LINE);
			this.writer.writeStartElement(lp);
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームに交信記録の末尾を書き込みます。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void foot() throws IOException {
		try {
			writer.writeCharacters(LINE);
			writer.writeEndElement();
			writer.writeCharacters(LINE);
			writer.writeEndDocument();
			writer.flush();
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームに書き込まずに交信記録を検査します。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void verify(Item item) throws IOException {
		try {
			final var rcvd = item.getRcvd();
			final var sent = item.getSent();
			for(var field: item) space(field.name());
			for(var field: sent) space(field.name());
			for(var field: sent) space(field.name());
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームの現在位置に交信記録を書き込みます。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @throws IOException 書き込みに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void output(Item item) throws IOException {
		final var lp = ITEM.getLocalPart();
		try {
			writer.writeCharacters(LINE);
			writer.writeStartElement(lp);
			for(var f: item) field(f);
			writer.writeCharacters(LINE);
			rcvd(item.getRcvd());
			sent(item.getSent());
			writer.writeEndElement();
			writer.flush();
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームに書き込むライタを返します。
	 *
	 *
	 * @return ライタ
	 *
	 * @throws IOException ライタの構築に失敗した場合
	 *
	 * @since 2020/09/05
	 */
	public final XMLStreamWriter writer() throws IOException {
		try {
			final var factor = XMLOutputFactory.newInstance();
			factor.setProperty(IS_REPAIRING_NAMESPACES, true);
			return factor.createXMLStreamWriter(target);
		} catch (XMLStreamException ex) {
			throw new IOException(ex);
		}
	}

	/**
	 * ストリームに名前空間の宣言を出力します。
	 *
	 *
	 * @param name 名前空間を使用する属性名
	 *
	 * @throws XMLStreamException 書き込みに失敗した場合
	 */
	private final void space(QName name) throws XMLStreamException {
		final var p = name.getPrefix();
		final var s = name.getNamespaceURI();
		if(!p.isEmpty() && spaces.add(p)) writer.writeNamespace(p, s);
	}

	/**
	 * ストリームに1件の相手局から受信した情報を出力します。
	 *
	 *
	 * @param rcvd 出力する相手局から受信した情報
	 *
	 * @throws XMLStreamException 書き込みに失敗した場合
	 */
	private final void rcvd(Rcvd rcvd) throws XMLStreamException {
		if(rcvd.iterator().hasNext()) {
			writer.writeEmptyElement(RCVD.getLocalPart());
			for(var f: rcvd) field(f);
			writer.writeCharacters(LINE);
		}
	}

	/**
	 * ストリームに1件の相手局まで送信した情報を出力します。
	 *
	 *
	 * @param sent 出力する相手局まで送信した情報
	 *
	 * @throws XMLStreamException 書き込みに失敗した場合
	 */
	private final void sent(Sent sent) throws XMLStreamException {
		if(sent.iterator().hasNext()) {
			writer.writeEmptyElement(SENT.getLocalPart());
			for(var f: sent) field(f);
			writer.writeCharacters(LINE);
		}
	}

	/**
	 * 属性を直列化してストリームに出力します。
	 *
	 *
	 * @param field 出力する属性
	 *
	 * @throws XMLStreamException 書き込みに失敗した場合
	 */
	private final void field(Field field) throws XMLStreamException {
		final var p = field.name().getPrefix();
		final var l = field.name().getLocalPart();
		final var u = field.name().getNamespaceURI();
		writer.writeAttribute(p, u, l, fields.encode(field));
	}
}
