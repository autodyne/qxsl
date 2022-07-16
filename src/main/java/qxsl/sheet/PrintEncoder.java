/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.UncheckedIOException;
import java.nio.charset.Charset;

/**
 * 印字可能な文字列の交信記録を書き出す機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public abstract class PrintEncoder extends BasicEncoder {
	private final Charset cset;

	/**
	 * 指定された名前と文字セットの書式を初期化します。
	 *
	 *
	 * @param name 書式の名前
	 * @param cset 文字セット
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	public PrintEncoder(String name, String cset) {
		super(name);
		this.cset = Charset.forName(cset);
	}

	/**
	 * 指定された属性と属性値を設定します。
	 *
	 *
	 * @param key 属性の名前
	 * @param val 属性の値
	 */
	@Override
	public final void set(String key, byte[] val) {
		set(key, new String(val, cset));
	}
}
