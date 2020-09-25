/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.io.Writer;

/**
 * 文字列による交信記録を書き出すためのエンコーダの共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/24
 */
public abstract class PrintEncoder extends TableEncoder {
	private final Writer writer;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 交信記録を書き込む出力
	 */
	public PrintEncoder(Writer writer) {
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
}
