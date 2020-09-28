/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.*;
import java.util.List;

import qxsl.model.Item;

/**
 * 交信記録を永続化する書式はこのインターフェースを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/25
 */
public abstract class TableFactory {
	/**
	 * この書式を識別する完全な名前を返します。
	 *
	 *
	 * @return 書式の名前
	 */
	public abstract String getName();

	/**
	 * この書式の表示に適した文字列を返します。
	 *
	 *
	 * @return 書式の文字列表現
	 */
	public abstract String toString();

	/**
	 * この書式の詳細を述べる文字列を返します。
	 *
	 *
	 * @return 書式の説明
	 */
	public abstract String getDescription();

	/**
	 * この書式の拡張子の不変リストを返します。
	 *
	 *
	 * @return 拡張子のリスト
	 */
	public abstract List<String> getExtensions();

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param reader 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public TableDecoder decoder(Reader reader) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param writer 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public TableEncoder encoder(Writer writer) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param is 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 */
	public abstract TableDecoder decoder(InputStream is);

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param os 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 */
	public abstract TableEncoder encoder(OutputStream os);

	/**
	 * 指定された文字列から交信記録を読み取ります。
	 *
	 *
	 * @param data 交信記録を読み込む文字列
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public final List<Item> decode(String data) throws IOException {
		return decoder(new StringReader(data)).decode();
	}

	/**
	 * 指定されたバイト列から交信記録を読み取ります。
	 *
	 *
	 * @param data 交信記録を読み込むバイト列
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外
	 */
	public final List<Item> decode(byte[] data) throws IOException {
		return decoder(new ByteArrayInputStream(data)).decode();
	}

	/**
	 * 指定された文字列から交信記録を読み取ります。
	 *
	 *
	 * @param data 交信記録を読み込む文字列
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public final Item decodeSingle(String data) throws IOException {
		return decode(data).get(0);
	}

	/**
	 * 指定されたバイト列から交信記録を読み取ります。
	 *
	 *
	 * @param data 交信記録を読み込むバイト列
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外
	 */
	public final Item decodeSingle(byte[] data) throws IOException {
		return decode(data).get(0);
	}

	/**
	 * 指定された交信記録をバイト列に書き出します。
	 *
	 *
	 * @param list 交信記録
	 *
	 * @return バイト列
	 *
	 * @throws IOException 書き込み時の例外
	 */
	public final byte[] encode(List<Item> list) throws IOException {
		try(final var out = new ByteArrayOutputStream()) {
			this.encoder(out).encode(list);
			return out.toByteArray();
		}
	}

	/**
	 * 指定された交信記録をバイト列に書き出します。
	 *
	 *
	 * @param sequence 交信記録
	 *
	 * @return バイト列
	 *
	 * @throws IOException 書き込み時の例外
	 */
	public final byte[] encode(Item...sequence) throws IOException {
		return encode(List.of(sequence));
	}
}
