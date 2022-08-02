/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Writer;
import java.time.format.DateTimeFormatter;

import qxsl.draft.Band;
import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.table.PrintEncoder;

import gaas.table.CqwwFactory.BandEnum;

import static java.time.ZoneOffset.UTC;

/**
 * 標準構造の交信記録をCabrillo書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/04
 */
public final class CqwwEncoder extends PrintEncoder {
	private static final String QSO = "QSO:";
	private final DateTimeFormatter tstamp;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 */
	public CqwwEncoder(Writer writer) {
		super("cqww", writer);
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
	public final void head() throws IOException {}

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
		space(QSO);
		band((Band) item.some(Qxsl.BAND));
		space(item.some(Qxsl.MODE).padTail(2));
		time((Time) item.some(Qxsl.TIME));
		space("*************");
		space(item.getSent().some(Qxsl.RSTQ).padTail(3));
		space(item.getSent().some(Qxsl.CODE).padTail(6));
		space(item.some(Qxsl.CALL).padTail(13));
		space(item.getRcvd().some(Qxsl.RSTQ).padTail(3));
		space(item.getRcvd().some(Qxsl.CODE).padTail(6));
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
		final var format = tstamp.withZone(UTC);
		if(date == null) space(" ".repeat(15));
		else space(format.format(date.value()));
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
		space(String.format("%5.5s", BandEnum.valueOf(band)));
	}
}
