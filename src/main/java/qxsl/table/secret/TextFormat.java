/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table.secret;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import qxsl.model.Field;

/**
 * プレインテキスト書式の交信記録用の{@link BaseFormat}です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2016/06/03
 *
 */
public abstract class TextFormat extends BaseFormat {
	/**
	 * プレインテキスト書式の交信記録を読み込むためのデコーダです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/24
	 *
	 */
	public static abstract class TextDecoder {
		private final LineNumberReader reader;
		private String line;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 * @param en 文字セット名
		 * @throws IOException 文字セットが利用できない場合
		 */
		public TextDecoder(InputStream in, String en) throws IOException {
			reader = new LineNumberReader(new InputStreamReader(in, en));
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 * 
		 * @throws IOException 入力エラーが発生した場合
		 */
		public final void close() throws IOException {
			reader.close();
		}

		/**
		 * 1行の文字列を読み込みます。終端の空白は除去されます。
		 * 
		 * @return 1行 ストリームが終端に達した場合null
		 * @throws IOException 入力エラーが発生した場合
		 */
		public final String readLine() throws IOException {
			if((line = reader.readLine()) == null) return null;
			char[] val = line.toCharArray();
			int len = val.length;
			while((0 < len) && (val[len - 1] <= ' ')) len--;
			if(len < val.length) line = line.substring(0, len);
			return line;
		}

		/**
		 * 現在の行を空白文字で分割して属性値を表す文字列の配列を返します。
		 * 
		 * @param numAttributes 属性の個数
		 * @return 切り出された字句の配列
		 */
		public final String[] getLine(int numAttributes) {
			return Arrays.copyOf(line.split(" +"), numAttributes);
		}

		/**
		 * 現在の行の文字列の指定された範囲を切り出して返します。
		 * 
		 * @param s 開始位置
		 * @param e 終了位置
		 * @return 切り出された字句
		 * @throws Exception 終了位置に空白でない文字がある場合
		 */
		public final String subLine(int s, int e) throws Exception {
			if(line.length() <= s) return "";
			if(e <= 0 || e >= line.length()) e = line.length();
			char next = e < line.length()? line.charAt(e): ' ';
			if(next == ' ') return line.substring(s, e).trim();
			throw parseError(e, "unexpected: " + line.substring(s));
		}

		/**
		 * 現在の行で発生した構文エラーを{@link IOException}で報告します。
		 * 
		 * @param cause エラーの原因
		 * @return  生成された例外
		 */
		public final IOException parseError(Throwable cause) {
			final String msg = cause.getMessage();
			final int ln = reader.getLineNumber();
			final String temp = "%s at line %d\n'%s'";
			return new IOException(String.format(temp, msg, ln, line), cause);
		}

		/**
		 * 現在の行で発生した構文エラーを{@link IOException}で報告します。
		 * 
		 * @param msg エラーメッセージ
		 * @return 生成された例外
		 */
		public final IOException parseError(String msg) {
			final int ln = reader.getLineNumber();
			final String temp = "%s at line %d\n'%s'";
			return new IOException(String.format(temp, msg, ln, line));
		}

		/**
		 * 現在の行で発生した構文エラーを{@link IOException}で報告します。
		 * 
		 * @param col エラーがある列
		 * @param cause エラーの原因
		 * @return  生成された例外
		 */
		public final IOException parseError(int col, Throwable cause) {
			final String msg = cause.getMessage();
			final int ln = reader.getLineNumber();
			final String temp = "%s at line %d column %d\n'%s'";
			return new IOException(String.format(temp, msg, ln, col, line));
		}

		/**
		 * 現在の行で発生した構文エラーを{@link IOException}で報告します。
		 * 
		 * @param col エラーがある列
		 * @param msg エラーメッセージ
		 * @return  生成された例外
		 */
		public final IOException parseError(int col, String msg) {
			final int ln = reader.getLineNumber();
			final String temp = "%s at line %d column %d\n'%s'";
			return new IOException(String.format(temp, msg, ln, col, line));
		}
	}

	/**
	 * 固定長プレインテキスト書式の交信記録を書き出すためのエンコーダです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/24
	 *
	 */
	public static abstract class TextEncoder {
		private final PrintStream stream;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 * @param en  文字セット
		 * @throws IOException 文字セットが利用できない場合
		 */
		public TextEncoder(OutputStream out, String en) throws IOException {
			stream = new PrintStream(out, true, en);
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 * 
		 * @throws IOException 出力エラーが発生した場合
		 */
		public final void close() throws IOException {
			stream.close();
		}

		/**
		 * 指定された文字を出力します。
		 * 
		 * @param ch 出力する文字
		 * @throws IOException 出力エラー発生時
		 */
		public final void print(char ch) throws IOException {
			stream.append(ch);
		}

		/**
		 * 指定された文字列を出力します。
		 * 
		 * @param s 出力する文字列
		 * @throws IOException 出力エラー発生時
		 */
		public final void print(String s) throws IOException {
			stream.print(s);
		}

		/**
		 * 改行を出力します。
		 * 
		 * @throws IOException 出力エラー発生時
		 */
		public final void println() throws IOException {
			stream.println();
		}

		/**
		 * 指定された文字列と改行を出力します。
		 * 
		 * @param s 出力する文字列
		 * @throws IOException 出力エラー発生時
		 */
		public final void println(String s) throws IOException {
			stream.println(s);
		}

		/**
		 * 指定された文字数の半角空白を出力します。
		 * 
		 * @param len 文字数
		 * @throws IOException 出力エラー発生時
		 */
		public final void printSpace(int len) throws IOException {
			char[] arr = new char[len];
			Arrays.fill(arr, ' ');
			stream.print(arr);
		}

		/**
		 * 指定された書式付き文字列を出力します。
		 * 
		 * @param f 出力する書式
		 * @param args  引数
		 * @throws IOException 出力エラー発生時
		 */
		public final void printf(String f, Object...args) throws IOException {
			stream.printf(f, args);
		}

		/**
		 * 指定された文字数まで空白文字で穴埋めした文字列を出力します。
		 * 
		 * @param len 文字列の長さ
		 * @param s 出力する文字列
		 * @throws IOException 出力エラー発生時
		 */
		public final void print(int len, String s) throws IOException {
			final String filled = String.format(String.format("%%%ds", len), s);
			final String msg = "'%s' is too long. consider shortening to '%s'.";
			if(filled.length() == len) stream.print(filled);
			else throw new IOException(String.format(msg, s, s.substring(0, len)));
		}

		/**
		 * 指定された文字数まで空白文字で穴埋めした{@link Field}を出力します。
		 * 
		 * @param len 文字列の長さ
		 * @param f 出力する{@link Field}
		 * @throws IOException 出力エラー発生時
		 */
		public final void print(int len, Field f) throws IOException {
			print(len, f != null? String.valueOf(f.value()) : "");
		}
	}
}
