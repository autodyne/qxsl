/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.IOException;

/**
 * 所定の書式の要約書類を所定の構造に読み込む仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/08
 */
public interface SheetDecoder extends AutoCloseable {
	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	public void close() throws IOException;

	/**
	 * 指定された属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性の値
	 */
	public byte[] getBinary(String key);

	/**
	 * 指定された属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性の値
	 */
	public String getString(String key);

	/**
	 * ストリームの要約書類を読み取ります。
	 *
	 *
	 * @return 要約書類を読み取ったデコーダ
	 *
	 * @throws IOException 構文上または読取り時の例外
	 */
	public SheetDecoder decode() throws IOException;
}
