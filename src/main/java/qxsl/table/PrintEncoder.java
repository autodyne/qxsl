/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.io.Writer;

/**
 * 印字可能な文字列の交信記録を書き出す機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/24
 */
public abstract class PrintEncoder extends BasicEncoder {
	private final Writer writer;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param type 書式の名前
	 * @param writer 交信記録を書き込む出力
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	public PrintEncoder(String type, Writer writer) {
		super(type);
		this.writer = writer;
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	@Override
	public final void close() throws IOException {
		writer.close();
	}

	/**
	 * 改行を出力します。
	 *
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	public final void println() throws IOException {
		writer.write(System.lineSeparator());
		writer.flush();
	}

	/**
	 * 指定された文字列を出力します。
	 *
	 *
	 * @param str 出力する文字列
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	public final void print(String str) throws IOException {
		writer.write(str);
	}

	/**
	 * 指定された文字列を空白付きで出力します。
	 *
	 *
	 * @param str 出力する文字列
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	public final void space(String str) throws IOException {
		writer.write(str);
		writer.write(' ');
	}

	/**
	 * 指定された文字列を空白付きで出力します。
	 * 空白文字列の場合はハイフンを出力します。
	 *
	 *
	 * @param str 出力する文字列
	 *
	 * @throws IOException 書き込みに失敗した場合
	 */
	public final void empty(String str) throws IOException {
		if(str.trim().isEmpty()) {
			writer.write('-');
			space(str.substring(1));
		} else {
			space(str);
		}
	}
}
