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
public interface TableFormat {
	/**
	 * この書式を識別する完全な名前を返します。
	 *
	 * @return 書式の名前
	 */
	public String getName();

	/**
	 * この書式の表示に適した文字列を返します。
	 *
	 * @return 書式の文字列表現
	 */
	public String toString();

	/**
	 * この書式の詳細を述べる文字列を返します。
	 *
	 * @return 書式の説明
	 */
	public String getDescription();

	/**
	 * この書式の拡張子の不変リストを返します。
	 *
	 * @return 拡張子のリスト
	 */
	public List<String> getExtensions();

	/**
	 * 指定されたストリームを入力とするデコーダを返します。
	 *
	 *
	 * @param is 交信記録を読み込むストリーム
	 *
	 * @return デコーダ
	 */
	public abstract TableDecoder decoder(InputStream is);

	/**
	 * 指定されたストリームに出力するエンコーダを返します。
	 *
	 *
	 * @param os 交信記録を出力するストリーム
	 *
	 * @return エンコーダ
	 */
	public abstract TableEncoder encoder(OutputStream os);

	/**
	 * 指定されたバイト列を入力とするデコーダを返します。
	 *
	 *
	 * @param data 交信記録を読み込むバイト列
	 *
	 * @return デコーダ
	 */
	public default TableDecoder decoder(byte[] data) {
		return decoder(new ByteArrayInputStream(data));
	}

	/**
	 * 指定されたリーダを入力とするデコーダを返します。
	 *
	 *
	 * @param reader 交信記録を読み込むリーダ
	 *
	 * @return デコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public default TableDecoder decoder(Reader reader) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定された文字列を入力とするデコーダを返します。
	 *
	 *
	 * @param data 交信記録を読み込む文字列
	 *
	 * @return デコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public default TableDecoder decoder(String data) {
		return decoder(new StringReader(data));
	}

	/**
	 * 指定されたライタに出力するデコーダを返します。
	 *
	 *
	 * @param writer 交信記録を書き出すライタ
	 *
	 * @return エンコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public default TableEncoder encoder(Writer writer) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定された交信記録をバイト列に書き出します。
	 *
	 *
	 * @param items 交信記録
	 *
	 * @return バイト列
	 *
	 * @throws UncheckedIOException 書き込み時の例外
	 */
	public default byte[] encode(List<Item> items) {
		final var out = new ByteArrayOutputStream();
		try(final var encoder = encoder(out)) {
			encoder.encode(items);
			return out.toByteArray();
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * 永続化された交信記録を読み込むためのデコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 */
	public interface TableDecoder extends AutoCloseable {
		/**
		 * ストリームから交信記録を読み出します。
		 *
		 * @return 読み出した交信記録
		 *
		 * @throws IOException 構文上または読取り時の例外
		 */
		public List<Item> decode() throws IOException;

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		public void close() throws IOException;
	}

	/**
	 * 交信記録を所定の書式で永続化するためのエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 */
	public interface TableEncoder extends AutoCloseable {
		/**
		 * ストリームに交信記録を書き込みます。
		 *
		 * @param items 交信記録
		 *
		 * @throws IOException 書き込み時の例外
		 */
		public void encode(List<Item> items) throws IOException;

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		public void close() throws IOException;
	}
}
