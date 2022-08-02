/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Writer;
import java.time.format.DateTimeFormatter;

import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.table.PrintEncoder;

/**
 * 標準構造の交信記録をCTESTWIN書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/07/02
 */
public final class CTxtEncoder extends PrintEncoder {
	private final DateTimeFormatter tstamp;
	private int count;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 */
	public CTxtEncoder(Writer writer) {
		super("ctxt", writer);
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
		print(String.format("Worked %4s stations", count()));
		println();
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
		space(String.format("%4d", ++count));
		time((Time) item.some(Qxsl.TIME));
		space(item.some(Qxsl.CALL).padTail(11));
		space(item.some(Qxsl.BAND).padHead(7));
		space(item.some(Qxsl.MODE).padTail(4));
		space(item.getSent().some(Qxsl.CODE).padTail(12));
		space(item.getRcvd().some(Qxsl.CODE).padTail(12));
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
	private void time(Time date) throws IOException {
		if(date == null) space(" ".repeat(10));
		else space(tstamp.format(date.local()));
	}
}
