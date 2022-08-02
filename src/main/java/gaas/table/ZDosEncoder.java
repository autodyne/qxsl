/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Writer;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

import qxsl.draft.Band;
import qxsl.draft.Name;
import qxsl.draft.Note;
import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.table.PrintEncoder;

/**
 * 標準構造の交信記録をzLogのDOS書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/25
 */
public final class ZDosEncoder extends PrintEncoder {
	private final DateTimeFormatter tstamp;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 */
	public ZDosEncoder(Writer writer) {
		super("zdos", writer);
		this.tstamp = getTimeEncoder();
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
		print(getHeaderText());
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
		space("");
		time((Time) item.some(Qxsl.TIME));
		space(item.some(Qxsl.CALL).padTail(10));
		space(item.getSent().some(Qxsl.CODE).padTail(12));
		space(item.getRcvd().some(Qxsl.CODE).padTail(12));
		space(item.some(Qxsl.MUL1).padTail(6));
		band((Band) item.some(Qxsl.BAND));
		space(item.some(Qxsl.MODE).padTail(4));
		space("1  ");
		name((Name) item.some(Qxsl.NAME));
		note((Note) item.some(Qxsl.NOTE));
		println();
	}

	/**
	 * 指定された日時を文字列として出力します。
	 *
	 *
	 * @param date 出力する日時
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	private final void time(Time date) throws IOException {
		if(date == null) space(" ".repeat(12));
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
		space(String.format("%5.5s", band.toDecimalString(3)));
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
