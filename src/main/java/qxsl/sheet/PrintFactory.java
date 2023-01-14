/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.sheet;

import java.io.*;
import java.nio.charset.Charset;

/**
 * 印字可能な文字列の要約書類を処理する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/24
 */
public abstract class PrintFactory extends BasicFactory {
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
	public PrintFactory(String name, String cset) {
		super(name);
		this.cset = Charset.forName(cset);
	}

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param reader 要約書類を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public abstract SheetDecoder decoder(Reader reader);

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param writer 要約書類を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public abstract SheetEncoder encoder(Writer writer);

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param is 要約書類を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public final SheetDecoder decoder(InputStream is) {
		return decoder(new InputStreamReader(is, cset));
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param os 要約書類を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public final SheetEncoder encoder(OutputStream os) {
		return encoder(new OutputStreamWriter(os, cset));
	}
}
