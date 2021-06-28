/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.*;
import java.util.List;
import java.util.StringJoiner;

/**
 * 交信記録を要約する書式はこのインターフェースを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public abstract class SheetFactory {
	/**
	 * この書式を識別する完全な名前を返します。
	 *
	 *
	 * @return 書式の名前
	 */
	public abstract String type();

	/**
	 * この書式の表示に適した文字列を返します。
	 *
	 *
	 * @return 書式の文字列表現
	 */
	public abstract String name();

	/**
	 * この書式の詳細を述べる文字列を返します。
	 *
	 *
	 * @return 書式の説明
	 */
	public abstract String help();

	/**
	 * この書式の拡張子の不変リストを返します。
	 *
	 *
	 * @return 拡張子のリスト
	 */
	public abstract List<String> extensions();

	/**
	 * 交信記録を抽出する鍵の文字列を返します。
	 *
	 *
	 * @return 交信記録を指す鍵
	 */
	public abstract String getTableKey();

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param reader 要約書類を読み込む入力
	 *
	 * @return デコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public SheetDecoder decoder(Reader reader) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param writer 要約書類を書き込む出力
	 *
	 * @return エンコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public SheetEncoder encoder(Writer writer) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param is 要約書類を読み込む入力
	 *
	 * @return デコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public SheetDecoder decoder(InputStream is) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param os 要約書類を書き込む出力
	 *
	 * @return エンコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public SheetEncoder encoder(OutputStream os) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定された要約書類を読み込むデコーダを返します。
	 *
	 *
	 * @param data 要約書類
	 *
	 * @return デコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public final SheetDecoder decoder(String data) {
		return decoder(new StringReader(data));
	}

	/**
	 * 指定された要約書類を読み込むデコーダを返します。
	 *
	 *
	 * @param data 要約書類
	 *
	 * @return デコーダ
	 */
	public final SheetDecoder decoder(byte[] data) {
		return decoder(new ByteArrayInputStream(data));
	}

	/**
	 * この書式のファイルフィルタへの表示に適した文字列を返します。
	 *
	 *
	 * @return 書式の文字列表現
	 */
	@Override
	public final String toString() {
		final var join = new StringJoiner(";", "|", "");
		for(var ext: extensions()) join.add("*.".concat(ext));
		return String.valueOf(name()).concat(join.toString());
	}
}
