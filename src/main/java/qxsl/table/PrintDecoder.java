/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.LinkedList;

/**
 * 文字列による交信記録を読み込むためのデコーダの共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/24
 */
public abstract class PrintDecoder extends TableDecoder {
	private static final int AHEAD_LIMIT = 1000;
	private final BufferedReader reader;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 交信記録を読み込む入力
	 */
	public PrintDecoder(Reader reader) {
		this.reader = new BufferedReader(reader);
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	@Override
	public final void close() throws IOException {
		reader.close();
	}

	/**
	 * 入力を直前の印付け位置まで戻します。
	 *
	 *
	 * @throws IOException 印付けが存在しない場合
	 */
	public final void reset() throws IOException {
		reader.reset();
	}

	/**
	 * 入力を消費せずに基本多言語面の文字を読み取ります。
	 *
	 *
	 * @return 文字
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	public final char peek() throws IOException {
		this.reader.mark(1);
		final int value = reader.read();
		this.reader.reset();
		return (char) value;
	}

	/**
	 * 現在の位置を記憶してから改行文字まで読み取ります。
	 *
	 *
	 * @return 切り出された文字列
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	public final String readLine() throws IOException {
		reader.mark(AHEAD_LIMIT);
		return reader.readLine();
	}

	/**
	 * 指定された文字列が書式を逸脱した場合に通知します。
	 *
	 *
	 * @param value 書式を逸脱した文字列
	 *
	 * @throws IOException 逸脱を知らせる例外
	 */
	private final void malform(String value) throws IOException {
		throw new IOException("malformed record: ".concat(value));
	}

	/**
	 * 改行文字まで読み取り、指定された位置で分割します。
	 * 分割位置に非空白文字がある場合に例外を発生します。
	 *
	 *
	 * @param cols 文字列を分割する位置
	 *
	 * @return 分割された文字列
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	public final String[] split(int... cols) throws IOException {
		final var list = new LinkedList<String>();
		final var line = this.readLine();
		for(int i = 1, j = 2; i < cols.length; i++, j++) {
			final int head = Math.min(cols[i - 1], line.length());
			final int last = Math.min(cols[i] - 1, line.length());
			if(j < cols.length && line.charAt(last) != ' ') break;
			list.add(line.substring(head, last).trim());
		}
		if(list.size() != cols.length - 1) malform(line);
		return list.toArray(new String[cols.length - 1]);
	}

	/**
	 * 指定された区切り文字列を発見するまで読み取ります。
	 * 区切り文字列のうち大文字と小文字は区別されません。
	 *
	 *
	 * @param last 区切り文字列
	 *
	 * @return 切り出された文字列
	 *
	 * @throws IOException 読み取りに失敗した場合
	 */
	public final String collect(String last) throws IOException {
		reader.mark(AHEAD_LIMIT);
		final var buf = new StringBuilder();
		int ch = 0, length = -last.length();
		while(Character.isDefined(ch = reader.read())) {
			buf.appendCodePoint(ch);
			final var suf = buf.substring(Math.max(0, ++length));
			if(suf.equalsIgnoreCase(last)) return buf.toString();
		}
		return null;
	}
}
