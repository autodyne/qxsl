/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.*;
import java.nio.charset.Charset;

/**
 * 印字可能な文字列で交信記録を表す書式の共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/05
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
	 * @param reader 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public abstract TableDecoder decoder(Reader reader);

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param writer 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public abstract TableEncoder encoder(Writer writer);

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param is 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public final TableDecoder decoder(InputStream is) {
		return decoder(new InputStreamReader(is, cset));
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param os 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public final TableEncoder encoder(OutputStream os) {
		return encoder(new OutputStreamWriter(os, cset));
	}
}
