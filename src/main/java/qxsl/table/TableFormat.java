/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.List;
import qxsl.model.Item;

/**
 * 交信記録を永続化する書式はこのインターフェースを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/25
 *
 */
public interface TableFormat {
	/**
	 * この書式を識別する完全な名前を返します。
	 *
	 * @return 書式の名前
	 */
	public String getName();

	/**
	 * この書式のUIへの表示に適した文字列を返します。
	 *
	 * @return 書式のUI文字列
	 */
	public String toString();

	/**
	 * この書式の詳細を説明する複数行の文字列を返します。
	 *
	 * @return 書式の説明
	 */
	public String getDescription();

	/**
	 * この書式のファイル名拡張子の不変のリストを返します。
	 *
	 * @return ファイル名拡張子のリスト
	 */
	public List<String> getExtensions();

	/**
	 * 指定されたストリームを入力とするデコーダを返します。
	 *
	 * @param is 交信記録を読み込むストリーム
	 * @return デコーダ
	 */
	public abstract TableDecoder decoder(InputStream is);

	/**
	 * 指定されたストリームに出力するエンコーダを返します。
	 *
	 * @param os 交信記録を出力するストリーム
	 * @return エンコーダ
	 */
	public abstract TableEncoder encoder(OutputStream os);

	/**
	 * 指定されたリーダを入力とするデコーダを返します。
	 * この操作の実装は任意です。
	 *
	 * @param reader 交信記録を読み込むリーダ
	 * @return デコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public default TableDecoder decoder(Reader reader) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 指定されたライタに出力するデコーダを返します。
	 * この操作の実装は任意です。
	 *
	 * @param writer 交信記録を書き出すライタ
	 * @return エンコーダ
	 *
	 * @throws UnsupportedOperationException 未実装の場合
	 */
	public default TableEncoder encoder(Writer writer) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 永続化された交信記録を読み込むためのデコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 *
	 */
	public interface TableDecoder extends AutoCloseable {
		/**
		 * ストリームから交信記録を読み出します。
		 *
		 * @return 読み出した交信記録
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
	 * 交信記録を書き出して永続化するためのエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 *
	 */
	public interface TableEncoder extends AutoCloseable {
		/**
		 * ストリームに交信記録を書き出します。
		 *
		 * @param items 交信記録
		 * @throws IOException 書き出し時の例外
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
