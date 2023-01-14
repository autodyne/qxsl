/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.sheet;

import java.io.UncheckedIOException;
import java.util.Properties;

import qxsl.utils.AssetUtil;

/**
 * 書式の説明を設定ファイルから取得する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public abstract class BasicEncoder implements SheetEncoder {
	private final Properties config;

	/**
	 * 指定された書式のエンコーダを初期化します。
	 *
	 *
	 * @param type 書式の名前
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	public BasicEncoder(String type) {
		this.config = AssetUtil.from(this).properties(type);
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
		return config.getProperty(key, "");
	}

	/**
	 * 交信記録を抽出する鍵の文字列を返します。
	 *
	 *
	 * @return 交信記録を指す鍵
	 */
	public final String getTableKey() {
		return get("table");
	}
}
