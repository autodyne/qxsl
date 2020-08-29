/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

import qxsl.model.Field;
import qxsl.table.TableFormat;

/**
 * このライブラリに内蔵される交信記録の書式の共通実装です。
 * 書式の説明を設定ファイルから取得する仕組みを提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 */
public abstract class BaseFormat implements TableFormat {
	private final String name;
	private final Properties conf;

	/**
	 * 各種設定を読み出して指定された名前の書式を構築します。
	 *
	 * @param name 書式の名前
	 *
	 * @throws UncheckedIOException 設定の読み出し時の例外
	 */
	public BaseFormat(String name) {
		this.name = name;
		this.conf = new Properties();
		install(String.format("%s.xml", name));
	}

	/**
	 * 指定された名前の設定ファイルを読み出します。
	 *
	 * @param name 設定ファイルの名前
	 *
	 * @throws UncheckedIOException 設定の読み出し時の例外
	 */
	private final void install(String name) {
		final var type = getClass();
		final var path = type.getResource(name);
		try(InputStream is = path.openStream()) {
			conf.loadFromXML(is);
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * この書式を識別する完全な名前を返します。
	 *
	 * @return 書式の名前
	 */
	@Override
	public final String getName() {
		return this.name;
	}

	/**
	 * この書式のUIへの表示に適した文字列を返します。
	 *
	 * @return 書式のUI文字列
	 */
	@Override
	public final String toString() {
		return conf.getProperty("label");
	}

	/**
	 * この書式の詳細を説明する複数行の文字列を返します。
	 *
	 * @return 書式の説明
	 */
	@Override
	public final String getDescription() {
		final String text = conf.getProperty("desc-text");
		final String file = conf.getProperty("desc-file");
		if(text != null) return text;
		try(var is = getClass().getResourceAsStream(file)) {
			var isr = new InputStreamReader(is, "UTF-8");
			var stream = new BufferedReader(isr).lines();
			return stream.collect(Collectors.joining("\n"));
		} catch (IOException ex) {
			return text;
		}
	}

	/**
	 * この書式を適用するファイル名拡張子の不変のリストを返します。
	 *
	 * @return ファイル名拡張子のリスト
	 */
	@Override
	public final List<String> getExtensions() {
		String[] exts = conf.getProperty("extensions").split(",");
		return Collections.unmodifiableList(Arrays.asList(exts));
	}

	/**
	 * この書式をファイルに出力する際のヘッダとなる文字列を返します。
	 *
	 * @return ヘッダの文字列
	 * @since 2019/07/11
	 */
	public final String getHeaderText() {
		return conf.getProperty("head-text").replaceAll("^\\R+|\\R+$", "");
	}

	/**
	 * 文字列による交信記録を読み込むためのデコーダの共通実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/24
	 */
	protected abstract class PlainTextDecoder implements TableDecoder {
		private final BufferedReader reader;

		/**
		 * 指定されたリーダを読み込むデコーダを構築します。
		 *
		 * @param reader 交信記録を読み込むリーダ
		 */
		public PlainTextDecoder(Reader reader) {
			this.reader = new BufferedReader(reader);
		}

		/**
		 * リーダを閉じてリソースを解放します。
		 *
		 * @throws IOException 解放の例外
		 */
		@Override
		public final void close() throws IOException {
			reader.close();
		}

		/**
		 * リーダの位置を直前のマーク位置まで戻します。
		 *
		 * @throws IOException マークが存在しない場合
		 */
		public final void reset() throws IOException {
			reader.reset();
		}

		/**
		 * 文字列を改行文字まで読み込みます。
		 * 読み込む直前の位置はマークされます。
		 *
		 * @return 切り出された文字列
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		public final String readLine() throws IOException {
			reader.mark(1000);
			return reader.readLine();
		}

		/**
		 * 改行文字まで読み込んで、指定された桁位置で分割します。
		 * 分割位置に空白以外の文字がある場合に例外を発生します。
		 *
		 * @param splits 文字列を分割する位置
		 *
		 * @return 分割された文字列
		 *
		 * @throws IOException 構文の問題もしくは読み込みに失敗した場合
		 */
		public final String[] split(int...splits) throws IOException {
			final var line = this.reader.readLine();
			final var list = new ArrayList<String>();
			final int len = splits.length - 1;
			for(int i = 1; i <= len; i++) {
				final int from = Math.min(splits[i - 1], line.length());
				final int last = Math.min(splits[i] - 1, line.length());
				if(last < line.length() && line.charAt(last) != ' ') break;
				else list.add(line.substring(from, last).trim());
			}
			if(list.size() == len) return list.toArray(new String[len]);
			throw new IOException(String.format("malformed: %s", line));
		}

		/**
		 * 指定された区切り文字列を発見するまで読み込みます。
		 * 区切り文字列のうち大文字と小文字は区別されません。
		 *
		 * @param delims 区切り文字列の選択肢となる配列
		 *
		 * @return 切り出された文字列
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		public final String split(String...delims) throws IOException {
			final StringBuilder sb = new StringBuilder();
			boolean seek = true;
			for(int index = 1; seek && reader.ready(); index++) {
				sb.appendCodePoint(reader.read());
				for(String delim: delims) try {
					final int from = index - delim.length();
					final String trail = sb.substring(from);
					if(trail.equalsIgnoreCase(delim)) seek = false;
				} catch (StringIndexOutOfBoundsException ex) {}
			}
			return seek? null: sb.toString();
		}
	}

	/**
	 * 文字列による交信記録を書き出すためのエンコーダの共通実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2013/06/24
	 */
	protected abstract class PlainTextEncoder implements TableEncoder {
		private final PrintWriter writer;

		/**
		 * 指定されたライタを読み込むデコーダを構築します。
		 *
		 * @param writer 交信記録を出力するライタ
		 */
		public PlainTextEncoder(Writer writer) {
			this.writer = new PrintWriter(writer, true);
		}

		/**
		 * ライタを閉じてリソースを解放します。
		 *
		 * @throws IOException 解放の例外
		 */
		@Override
		public final void close() throws IOException {
			writer.close();
		}

		/**
		 * 指定された文字列を出力します。
		 *
		 * @param s 出力する文字列
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		public final void print(String s) throws IOException {
			writer.print(s);
		}

		/**
		 * 改行を出力します。
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		public final void println() throws IOException {
			writer.println();
		}

		/**
		 * 指定された書式付き文字列を出力します。
		 *
		 * @param f 出力する書式
		 * @param v 引数
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		public final void printf(String f, Object...v) throws IOException {
			writer.printf(f, v);
		}

		/**
		 * 指定された文字数まで右詰で穴埋めした{@link Field}を出力します。
		 *
		 * @param len 文字列の長さ
		 * @param f 出力する属性
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		public final void printR(int len, Field f) throws IOException {
			printR(len, f != null? String.valueOf(f.value()) : "");
		}

		/**
		 * 指定された文字数まで左詰で穴埋めした{@link Field}を出力します。
		 *
		 * @param len 文字列の長さ
		 * @param f 出力する属性
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		public final void printL(int len, Field f) throws IOException {
			printL(len, f != null? String.valueOf(f.value()) : "");
		}

		/**
		 * 指定された文字数まで空白文字で穴埋めした文字列を右詰で出力します。
		 *
		 * @param len 文字列の長さ
		 * @param s 出力する文字列
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		public final void printR(int len, String s) throws IOException {
			final var str = String.format(String.format("%%%ds", len), s);
			final var msg = "value '%s' must be truncated into length %d";
			if(str.length() == len) writer.print(str);
			else throw new IOException(String.format(str, s, len));
		}

		/**
		 * 指定された文字数まで空白文字で穴埋めした文字列を左詰で出力します。
		 *
		 * @param len 文字列の長さ
		 * @param s 出力する文字列
		 *
		 * @throws IOException 書き込みに失敗した場合
		 */
		public final void printL(int len, String s) throws IOException {
			final var str = String.format(String.format("%%-%ds", len), s);
			final var msg = "value '%s' must be truncated into length %d";
			if(str.length() == len) writer.print(str);
			else throw new IOException(String.format(str, s, len));
		}
	}
}
