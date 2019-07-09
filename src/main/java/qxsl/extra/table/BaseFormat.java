/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.net.URL;
import java.nio.charset.Charset;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import qxsl.model.Field;
import qxsl.model.Item;
import qxsl.table.TableFormat;

/**
 * このライブラリに内蔵される交信記録の書式の共通実装です。
 * 書式の説明を設定ファイルから取得する仕組みを提供します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/16
 *
 */
public abstract class BaseFormat implements TableFormat {
	private final String name;
	private final Properties conf;

	/**
	 * 指定された名前の書式を構築します。
	 *
	 * @param name 書式の名前
	 */
	public BaseFormat(String name) {
		this.name = name;
		this.conf = new Properties();
		String file = String.format("%s.xml", name);
		URL url = this.getClass().getResource(file);
		try(InputStream is = url.openStream()) {
			conf.loadFromXML(is);
		} catch(IOException ex) {}
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
		try(InputStream is = getClass().getResourceAsStream(file)) {
			final Reader isr = new InputStreamReader(is, "UTF-8");
			Stream<String> strm = new BufferedReader(isr).lines();
			return strm.collect(Collectors.joining("\n"));
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
	 * プレインテキストの交信記録を読み込むためのデコーダの共通実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/24
	 *
	 */
	protected abstract class PlainTextDecoder implements TableDecoder {
		private final BufferedReader reader;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param is 交信記録を読み込むストリーム
		 * @param cs 文字セット
		 */
		public PlainTextDecoder(InputStream is, Charset cs) {
			this.reader = new BufferedReader(new InputStreamReader(is, cs));
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 * 
		 * @throws IOException リソース解放に失敗した場合
		 */
		@Override
		public final void close() throws IOException {
			reader.close();
		}

		/**
		 * ストリームの位置を直前のマーク位置まで戻します。
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
		 * @return 分割された文字列
		 *
		 * @throws IOException 構文の問題もしくは読み込みに失敗した場合
		 */
		public final String[] splitLine(int...splits) throws IOException {
			final String line = this.reader.readLine();
			final List<String> list = new ArrayList<>();
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
		 * @return 切り出された文字列
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		public final String tokenize(String...delims) throws IOException {
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
	 * プレインテキストの交信記録を書き出すためのエンコーダの共通実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/24
	 *
	 */
	protected abstract class PlainTextEncoder implements TableEncoder {
		private final PrintStream stream;

		/**
		 * 指定されたストリームを読み込むデコーダを構築します。
		 * 
		 * @param os 交信記録を出力するストリーム
		 * @param cs 文字セット
		 */
		public PlainTextEncoder(OutputStream os, Charset cs) {
			this.stream = new PrintStream(os, true, cs);
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 * 
		 * @throws IOException リソース解放に失敗した場合
		 */
		@Override
		public final void close() throws IOException {
			stream.close();
		}

		/**
		 * リソースファイルで設定されたこの書式のヘッダ部を出力します。
		 * 
		 * @param file リソースファイルへのパス
		 * @throws IOException 出力例外発生時
		 */
		public final void printHead(String file) throws IOException {
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
		 * @throws IOException 出力例外発生時
		 */
		public final void print(String s) throws IOException {
			stream.print(s);
		}

		/**
		 * 改行を出力します。
		 * 
		 * @throws IOException 出力例外発生時
		 */
		public final void println() throws IOException {
			stream.println();
		}

		/**
		 * 指定された書式付き文字列を出力します。
		 * 
		 * @param f 出力する書式
		 * @param args  引数
		 * @throws IOException 出力例外発生時
		 */
		public final void printf(String f, Object...args) throws IOException {
			stream.printf(f, args);
		}

		/**
		 * 指定された文字数まで空白文字で穴埋めした文字列を右詰で出力します。
		 * 
		 * @param len 文字列の長さ
		 * @param s 出力する文字列
		 * @throws IOException 出力例外発生時
		 */
		public final void printR(int len, String s) throws IOException {
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
		 * @throws IOException 出力例外発生時
		 */
		public final void printL(int len, String s) throws IOException {
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
		 * @throws IOException 出力例外発生時
		 */
		public final void printR(int len, Field f) throws IOException {
			printR(len, f != null? String.valueOf(f.value()) : "");
		}

		/**
		 * 指定された文字数まで左詰で穴埋めした{@link Field}を出力します。
		 * 
		 * @param len 文字列の長さ
		 * @param f 出力する{@link Field}
		 * @throws IOException 出力例外発生時
		 */
		public final void printL(int len, Field f) throws IOException {
			printL(len, f != null? String.valueOf(f.value()) : "");
		}
	}
}
