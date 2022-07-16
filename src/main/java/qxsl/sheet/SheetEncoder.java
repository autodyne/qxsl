/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.IOException;

/**
 * 所定の構造の要約書類を所定の書式に書き出す仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/08
 */
public interface SheetEncoder extends AutoCloseable {
	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	public void close() throws IOException;

	/**
	 * 指定された属性と属性値を設定します。
	 *
	 *
	 * @param key 属性の名前
	 * @param val 属性の値
	 */
	public void set(String key, byte[] val);

	/**
	 * 指定された属性と属性値を設定します。
	 *
	 *
	 * @param key 属性の名前
	 * @param val 属性の値
	 */
	public void set(String key, String val);

	/**
	 * ストリームに要約書類を書き込みます。
	 *
	 *
	 * @throws IOException 書き込み時の例外
	 */
	public void encode() throws IOException;
}
