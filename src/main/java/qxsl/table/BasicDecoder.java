/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.UncheckedIOException;
import java.time.Year;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Properties;

import qxsl.utils.AssetUtil;

/**
 * 書式の説明を設定ファイルから取得する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/16
 */
public abstract class BasicDecoder extends TableDecoder {
	private final Properties conf;

	/**
	 * 指定された書式のデコーダを初期化します。
	 *
	 *
	 * @param type 書式の名前
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	public BasicDecoder(String type) {
		this.conf = AssetUtil.from(this).properties(type);
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
		return conf.getProperty(key, "");
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
}
