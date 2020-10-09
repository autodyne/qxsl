/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Writer;
import java.time.format.DateTimeFormatter;

import qxsl.model.Item;
import qxsl.table.PrintEncoder;
import qxsl.value.Field;

import gaas.field.Band;
import gaas.field.Qxsl;
import gaas.field.Time;
import gaas.table.CqwwFactory.BandEnum;

import static java.time.ZoneOffset.UTC;

/**
 * 交信記録をCabrillo書式に直列化するエンコーダです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/04
 */
public final class CqwwEncoder extends PrintEncoder {
	private final DateTimeFormatter tstamp;
	private final CqwwFactory format;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 * @param format 書式
	 */
	public CqwwEncoder(Writer writer, CqwwFactory format) {
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
		if(fld.name().equals(Qxsl.BAND)) return;
		if(fld.name().equals(Qxsl.MODE)) return;
		if(fld.name().equals(Qxsl.TIME)) return;
		if(fld.name().equals(Qxsl.RSTQ)) return;
		if(fld.name().equals(Qxsl.CODE)) return;
		if(fld.name().equals(Qxsl.CALL)) return;
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
		space(format.QSO);
		band((Band) item.get(Qxsl.BAND));
		space(item.get(Qxsl.MODE).padHead(2));
		time((Time) item.get(Qxsl.TIME));
		space("*************");
		space(item.getSent().get(Qxsl.RSTQ).padHead(3));
		space(item.getSent().get(Qxsl.CODE).padHead(6));
		space(item.get(Qxsl.CALL).padHead(13));
		space(item.getRcvd().get(Qxsl.RSTQ).padHead(3));
		space(item.getRcvd().get(Qxsl.CODE).padHead(6));
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
