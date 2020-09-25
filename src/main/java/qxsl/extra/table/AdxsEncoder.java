/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.IOException;
import java.io.Writer;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import qxsl.field.FieldManager;
import qxsl.model.Field;
import qxsl.model.Item;
import qxsl.table.TableEncoder;

import static qxsl.extra.table.AdxsFactory.*;

import static javax.xml.stream.XMLOutputFactory.IS_REPAIRING_NAMESPACES;

/**
 * 交信記録をADX書式に直列化するエンコーダです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/08
 */
public final class AdxsEncoder extends TableEncoder {
	private final FieldManager fields;
	private final AdxsFactory format;
	private final Writer target;
	private XMLStreamWriter writer;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 * @param format 書式
	 */
	public AdxsEncoder(Writer writer, AdxsFactory format) {
		this.fields = new FieldManager();
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
		final var rt = ROOT.getLocalPart();
		final var hd = HEAD.getLocalPart();
		final var ls = LIST.getLocalPart();
		try {
			this.writer = writer();
			this.writer.writeStartDocument();
			this.writer.writeCharacters(LINE);
			this.writer.writeStartElement(rt);
			this.writer.writeCharacters(LINE);
			this.writer.writeStartElement(hd);
			this.writer.writeEndElement();
			this.writer.writeCharacters(LINE);
			this.writer.writeStartElement(ls);
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
		for(var f: item) verify(f);
		for(var f: item.getRcvd()) verify(f);
		for(var f: item.getSent()) verify(f);
	}

	/**
	 * ストリームに書き込まずに属性を検査します。
	 *
	 *
	 * @param fld 属性
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2020/09/04
	 */
	private final void verify(Field fld) throws IOException {
		if(!NURI.equals(fld.name().getNamespaceURI())) {
			final var str = "field element '%s' is not supported";
			throw new IOException(String.format(str, fld.name()));
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
			for(var f: item.getRcvd()) field(f);
			for(var f: item.getSent()) field(f);
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
	 * 属性を直列化してストリームに出力します。
	 *
	 *
	 * @param field 出力する属性
	 *
	 * @throws XMLStreamException 書き込みに失敗した場合
	 */
	private final void field(Field field) throws XMLStreamException {
		writer.writeStartElement(field.name().getLocalPart());
		writer.writeCharacters(fields.encode(field));
		writer.writeEndElement();
		writer.writeCharacters(LINE);
	}
}
