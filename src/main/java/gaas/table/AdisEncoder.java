/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Writer;
import java.time.ZonedDateTime;

import qxsl.field.FieldManager;
import qxsl.model.Item;
import qxsl.table.PrintEncoder;
import qxsl.value.Field;

import static gaas.table.AdisFactory.EOH;
import static gaas.table.AdisFactory.EOR;

import static java.time.format.DateTimeFormatter.ofPattern;

/**
 * 標準構造の交信記録をADI書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/08
 */
public final class AdisEncoder extends PrintEncoder {
	private final FieldManager fields;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 */
	public AdisEncoder(Writer writer) {
		super("adis", writer);
		this.fields = new FieldManager();
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
		final var head = getHeaderText();
		final var stmp = ofPattern(get("created"));
		final var time = ZonedDateTime.now().format(stmp);
		print(String.format(head, time.length(), time));
		print(EOH);
		println();
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
	public final void foot() throws IOException {}

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
		for(var f: item) field(f);
		print(EOR);
		println();
	}

	/**
	 * 指定された属性の値をストリームに書き込みます。
	 *
	 *
	 * @param field 書き込む属性
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void field(Field field) throws IOException {
		final var local = field.name().getLocalPart();
		final var value = fields.encode(field);
		print(String.format("<%s:%d>%s", local, value.length(), value));
	}
}
