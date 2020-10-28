/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.UncheckedIOException;
import java.time.Year;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.List;
import java.util.Properties;

import gaas.utils.AssetUtils;

/**
 * 書式の説明を設定ファイルから取得する仕組みを提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 */
public abstract class BasicFactory extends TableFactory {
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
		this.conf = AssetUtils.from(this).properties(name);
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
		return AssetUtils.from(this).string(file);
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
	 * この書式のヘッダとなる文字列を返します。
	 *
	 *
	 * @return ヘッダの文字列
	 *
	 * @since 2019/07/11
	 */
	public final String getHeaderText() {
		return get("head-text").replaceAll("^\\R+|\\R+$", "");
	}

	/**
	 * この書式の入力時に使う時刻の書式を返します。
	 *
	 *
	 * @return 時刻の書式
	 *
	 * @since 2020/09/06
	 */
	public final DateTimeFormatter getTimeDecoderOld() {
		final var pattern = get("time-decoder");
		final int current = Year.now().getValue();
		final var factory = new DateTimeFormatterBuilder();
		factory.parseDefaulting(ChronoField.YEAR, current);
		return factory.appendPattern(pattern).toFormatter();
	}

	/**
	 * この書式の出力時に使う時刻の書式を返します。
	 *
	 *
	 * @return 時刻の書式
	 *
	 * @since 2020/09/06
	 */
	public final DateTimeFormatter getTimeDecoder() {
		return DateTimeFormatter.ofPattern(get("time-decoder"));
	}

	/**
	 * この書式の出力時に使う時刻の書式を返します。
	 *
	 *
	 * @return 時刻の書式
	 *
	 * @since 2020/09/06
	 */
	public final DateTimeFormatter getTimeEncoder() {
		return DateTimeFormatter.ofPattern(get("time-encoder"));
	}
}
