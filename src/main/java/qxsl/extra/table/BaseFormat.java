/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.table;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import qxsl.table.TableFormat;

/**
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
	 * 指定した名前の書式を構築します。
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
	 * ストリームの内容をメモリに展開して何度でも走査可能にするストリームです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/04/02
	 *
	 */
	public static final class ReusableInputStream extends InputStream {
		private final ByteArrayInputStream data;

		/**
		 * 指定されたストリームをメモリに展開します。
		 *
		 * @param input 読み込むストリーム
		 * @throws IOException 読み込みに失敗した場合
		 */
		public ReusableInputStream(InputStream input) throws IOException {
			final ByteArrayOutputStream bout = new ByteArrayOutputStream();
			final byte[] buffer = new byte[1024];
			int len = 0;
			try (InputStream s = input) {
				while((len = s.read(buffer)) > 0) bout.write(buffer, 0, len);
			}
			this.data = new ByteArrayInputStream(bout.toByteArray());
		}

		@Override
		public int available() {
			return data.available();
		}

		@Override
		public int read() {
			return data.read();
		}

		@Override
		public void reset() {
			data.reset();
		}
	}
}