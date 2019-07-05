/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.table;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Reader;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import qxsl.model.Field;
import qxsl.table.TableFormat;

/**
 * プレインテキスト符号化による{@link TableFormat}の共通実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2016/06/03
 *
 */
public abstract class TextFormat extends BaseFormat implements TableFormat {
	private final Charset charset;

	/**
	 * 指定された名前の書式を指定された文字セットで構築します。
	 *
	 * @param name 書式の名前
	 * @param cset 文字セット名
	 */
	public TextFormat(String name, String cset) {
		super(name);
		this.charset = Charset.forName(cset);
	}

	/**
	 * プレインテキスト書式の交信記録を読み込むためのデコーダです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/24
	 *
	 */
	protected abstract class TextDecoder implements AutoCloseable {
		private final BufferedReader reader;
		private String line;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param in 読み込むストリーム
		 */
		public TextDecoder(InputStream in) {
			reader = new BufferedReader(new InputStreamReader(in, charset));
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 * 
		 * @throws IOException 入力エラーが発生した場合
		 */
		public void close() throws IOException {
			reader.close();
		}

		/**
		 * 文字列を改行文字まで読み込みます。
		 *
		 * @return 切り出された文字列
		 *
		 * @throws IOException 読み込み時の例外
		 */
		public String readLine() throws IOException {
			return reader.readLine();
		}

		/**
		 * 指定された区切り文字列を発見するまで読み込みます。
		 * 区切り文字列のうち大文字と小文字は区別されません。
		 *
		 * @param delims 区切り文字列の選択肢となる配列
		 * @return 切り出された文字列
		 *
		 * @throws IOException 読み込み時の例外
		 */
		public String split(String...delims) throws IOException {
			final StringBuilder sb = new StringBuilder();
			for(int index = 0, start = 0; true; index++) {
				final int code = reader.read();
				if(code == -1) break;
				sb.append(code);
				for(String delim: delims) {
					final int from = index - delim.length();
					final String trail = sb.substring(from);
					if(trail.equalsIgnoreCase(delim)) break;
				}
			}
			return sb.toString();
		}

		/**
		 * 指定された文字列を指定された桁位置で分割します。
		 * 分割位置に空白以外の文字がある場合に例外を発生します。
		 *
		 * @param splits 文字列を分割する位置
		 * @return 分割された文字列
		 *
		 * @throws IOException 読み込み時の例外または構文上の例外
		 */
		public String[] split(String line, int...splits) throws IOException {
			final List<String> list = new ArrayList<>();
			boolean valid = true;
			for(int i = 0; i < splits.length - 1; i++) try {
				final int from = splits[i];
				final int last = splits[i+1];
				if(last >= 0) list.add(line.substring(from, last).trim());
				else list.add(line.substring(from, line.length()).trim());
				if(last > 0 && line.charAt(last - 1) != ' ') valid = false;
			} catch (IndexOutOfBoundsException ex) {
				valid = false;
			}
			if(valid) return list.toArray(new String[list.size()]);
			throw new IOException(String.format("unexpected: '%s'", line));
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
	public abstract class TextEncoder implements AutoCloseable {
		private final PrintStream stream;

		/**
		 * 指定されたストリームに出力するエンコーダを構築します。
		 * 
		 * @param out 交信記録を出力するストリーム
		 */
		public TextEncoder(OutputStream out) {
			stream = new PrintStream(out, true, charset);
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 * 
		 * @throws IOException 出力エラーが発生した場合
		 */
		public void close() throws IOException {
			stream.close();
		}

		/**
		 * リソースファイルで設定されたこの書式のヘッダ部を出力します。
		 * このメソッドは規定されていない改行文字を独自に出力しません。
		 * 
		 * @throws IOException 出力エラー発生時
		 */
		public void printHead() throws IOException {
			final String file = getName().concat(".fmt");
			final URL url = getClass().getResource(file);
			try(InputStream is = url.openStream()) {
				final Reader isr = new InputStreamReader(is, "UTF-8");
				Stream<String> strm = new BufferedReader(isr).lines();
				stream.print(strm.collect(Collectors.joining("\n")));
			}
		}

		/**
		 * 指定された文字列を出力します。
		 * 
		 * @param s 出力する文字列
		 * @throws IOException 出力エラー発生時
		 */
		public void print(String s) throws IOException {
			stream.print(s);
		}

		/**
		 * 改行を出力します。
		 * 
		 * @throws IOException 出力エラー発生時
		 */
		public void println() throws IOException {
			stream.println();
		}

		/**
		 * 指定された文字列と改行を出力します。
		 * 
		 * @param s 出力する文字列
		 * @throws IOException 出力エラー発生時
		 */
		public void println(String s) throws IOException {
			stream.println(s);
		}

		/**
		 * 指定された文字数の半角空白を出力します。
		 * 
		 * @param len 文字数
		 * @throws IOException 出力エラー発生時
		 */
		public void printSpace(int len) throws IOException {
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
		public void printf(String f, Object...args) throws IOException {
			stream.printf(f, args);
		}

		/**
		 * 指定された文字数まで空白文字で穴埋めした文字列を右詰で出力します。
		 * 
		 * @param len 文字列の長さ
		 * @param s 出力する文字列
		 * @throws IOException 出力エラー発生時
		 */
		public void printR(int len, String s) throws IOException {
			final String filled = String.format(String.format("%%%ds", len), s);
			final String msg = "'%s' is too long (consider shortening to '%s').";
			if(filled.length() == len) stream.print(filled);
			else throw new IOException(String.format(msg, s, s.substring(0, len)));
		}

		/**
		 * 指定された文字数まで空白文字で穴埋めした文字列を左詰で出力します。
		 * 
		 * @param len 文字列の長さ
		 * @param s 出力する文字列
		 * @throws IOException 出力エラー発生時
		 */
		public void printL(int len, String s) throws IOException {
			final String filled = String.format(String.format("%%-%ds", len), s);
			final String msg = "'%s' is too long (consider shortening to '%s').";
			if(filled.length() == len) stream.print(filled);
			else throw new IOException(String.format(msg, s, s.substring(0, len)));
		}

		/**
		 * 指定された文字数まで右詰で穴埋めした{@link Field}を出力します。
		 * 
		 * @param len 文字列の長さ
		 * @param f 出力する{@link Field}
		 * @throws IOException 出力エラー発生時
		 */
		public void printR(int len, Field f) throws IOException {
			printR(len, f != null? String.valueOf(f.value()) : "");
		}

		/**
		 * 指定された文字数まで左詰で穴埋めした{@link Field}を出力します。
		 * 
		 * @param len 文字列の長さ
		 * @param f 出力する{@link Field}
		 * @throws IOException 出力エラー発生時
		 */
		public void printL(int len, Field f) throws IOException {
			printL(len, f != null? String.valueOf(f.value()) : "");
		}
	}
}
