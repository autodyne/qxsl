/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * 書式の説明を設定ファイルから取得する仕組みを提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 */
public abstract class BasicFactory extends SheetFactory {
	private final String name;
	private final Properties conf;

	/**
	 * 指定された名前の書式を初期化します。
	 *
	 *
	 * @param name 書式の名前
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	public BasicFactory(String name) {
		this.name = name;
		this.conf = new Properties();
		install(String.format("%s.xml", name));
	}

	/**
	 * 指定された書式の設定を読み取ります。
	 *
	 *
	 * @param name 書式の名前
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	private final void install(String name) {
		final var type = getClass();
		final var path = type.getResource(name);
		try(final var strm = path.openStream()) {
			conf.loadFromXML(strm);
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * 指定された名前の設定の値を返します。
	 *
	 *
	 * @param key 設定の名前
	 *
	 * @return 設定の値
	 */
	public final String get(String key) {
		return conf.getProperty(key);
	}

	/**
	 * この書式を識別する完全な名前を返します。
	 *
	 *
	 * @return 書式の名前
	 */
	@Override
	public final String getName() {
		return name;
	}

	/**
	 * この書式の表示に適した文字列を返します。
	 *
	 *
	 * @return 書式の文字列表現
	 */
	@Override
	public final String toString() {
		return get("label");
	}

	/**
	 * この書式の詳細を述べる文字列を返します。
	 *
	 *
	 * @return 書式の説明
	 */
	@Override
	public final String getDescription() {
		final var text = get("desc-text");
		final var file = get("desc-file");
		if(text != null) return text;
		return getResourceAsString(file);
	}

	/**
	 * この書式の拡張子の不変リストを返します。
	 *
	 *
	 * @return 拡張子のリスト
	 */
	@Override
	public final List<String> getExtensions() {
		return List.of(get("extensions").split(","));
	}

  /**
   * 交信記録を抽出する鍵の文字列を返します。
   *
	 *
   * @return 交信記録を指す鍵
   */
	@Override
  public final String getTableKey() {
		return get("table");
	}

	/**
	 * 指定されたリソースの内容を読み取ります。
	 *
	 *
	 * @param path ファイルのパス
	 *
	 * @return ファイルの内容
	 *
	 * @throws UncheckedIOException 内容の取得時の例外
	 *
	 * @since 2020/09/05
	 */
	public final String getResourceAsString(String path) {
		try(var is = getClass().getResourceAsStream(path)) {
			final var isr = new InputStreamReader(is, "UTF-8");
			final var stream = new BufferedReader(isr).lines();
			return stream.collect(Collectors.joining("\n"));
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}
}
