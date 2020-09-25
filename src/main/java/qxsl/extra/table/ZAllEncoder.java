/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.IOException;
import java.io.Writer;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

import qxsl.extra.field.Band;
import qxsl.extra.field.Name;
import qxsl.extra.field.Note;
import qxsl.extra.field.Qxsl;
import qxsl.extra.field.Time;
import qxsl.model.Field;
import qxsl.model.Item;
import qxsl.table.PrintEncoder;

/**
 * 交信記録をzLog ALL書式に直列化するエンコーダです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/23
 */
public final class ZAllEncoder extends PrintEncoder {
	private final DateTimeFormatter tstamp;
	private final ZAllFactory format;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 * @param format 書式
	 */
	public ZAllEncoder(Writer writer, ZAllFactory format) {
		super(writer);
		this.format = format;
		this.tstamp = format.getTimeEncoder();
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
		print(format.getHeaderText());
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
		if(fld.name().equals(Qxsl.TIME)) return;
		if(fld.name().equals(Qxsl.CALL)) return;
		if(fld.name().equals(Qxsl.RSTQ)) return;
		if(fld.name().equals(Qxsl.CODE)) return;
		if(fld.name().equals(Qxsl.BAND)) return;
		if(fld.name().equals(Qxsl.MODE)) return;
		if(fld.name().equals(Qxsl.NAME)) return;
		if(fld.name().equals(Qxsl.NOTE)) return;
		final var str = "field element '%s' is not supported";
		throw new IOException(String.format(str, fld.name()));
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
		time((Time) item.get(Qxsl.TIME));
		space(item.get(Qxsl.CALL).padTail(12));
		space(item.getSent().get(Qxsl.RSTQ).padTail(3));
		space(item.getSent().get(Qxsl.CODE).padTail(7));
		space(item.getRcvd().get(Qxsl.RSTQ).padTail(3));
		space(item.getRcvd().get(Qxsl.CODE).padTail(7));
		space("-     -    ");
		band((Band) item.get(Qxsl.BAND));
		space(item.get(Qxsl.MODE).padTail(4));
		space("1 ");
		name((Name) item.get(Qxsl.NAME));
		note((Note) item.get(Qxsl.NOTE));
		println();
	}

	/**
	 * 指定された交信日時を文字列として出力します。
	 *
	 *
	 * @param date 出力する交信日時
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void time(Time date) throws IOException {
		if(date == null) space(" ".repeat(16));
		else space(tstamp.format(date.local()));
	}

	/**
	 * 指定された周波数帯を文字列として出力します。
	 *
	 *
	 * @param band 出力する周波数帯
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void band(Band band) throws IOException {
		space(String.format("%4.4s", band.toDecimalString(3)));
	}

	/**
	 * 指定された運用者名を文字列として出力します。
	 *
	 *
	 * @param op 出力する運用者名
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void name(Name op) throws IOException {
		space("%%".concat(Objects.toString(op, "")).concat("%%"));
	}

	/**
	 * 指定された備考を文字列として出力します。
	 *
	 *
	 * @param note 出力する備考
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void note(Note note) throws IOException {
		if(note != null) print(note.value());
	}
}
