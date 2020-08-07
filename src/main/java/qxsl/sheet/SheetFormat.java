/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.List;
import java.util.Map;

/**
 * 交信記録を要約する書式はこのインターフェースを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public interface SheetFormat {
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
	 * この書式で交信記録を抽出する鍵文字列を返します。
	 *
	 * @return 交信記録を指す鍵
	 */
	public String getTableKey();

	/**
	 * 指定されたリーダを入力とするデコーダを返します。
	 *
	 *
	 * @param reader 要約書類を読み込むリーダ
	 *
	 * @return デコーダ
	 */
	public abstract SheetDecoder decoder(Reader reader);

	/**
	 * 指定されたライタに出力するエンコーダを返します。
	 *
	 *
	 * @param writer 要約書類を書き出すライタ
	 *
	 * @return エンコーダ
	 */
	public abstract SheetEncoder encoder(Writer writer);

	/**
	 * 指定された文字列を入力とするデコーダを返します。
	 *
	 *
	 * @param data 要約書類を読み込む文字列
	 *
	 * @return デコーダ
	 */
	public default SheetDecoder decoder(String data) {
		return decoder(new StringReader(data));
	}

	/**
	 * 永続化された要約書類を読み込むためのデコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 */
	public interface SheetDecoder extends AutoCloseable {
		/**
		 * ストリームから要約書類を読み出します。
		 *
		 *
		 * @return 読み出した要約書類
		 *
		 * @throws IOException 構文上または読取り時の例外
		 */
		public Map<String, String> decode() throws IOException;

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		public void close() throws IOException;
	}

	/**
	 * 要約書類を所定の書式で永続化するためのエンコーダです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 */
	public interface SheetEncoder extends AutoCloseable {
		/**
		 * ストリームに要約書類を書き込みます。
		 *
		 *
		 * @param map 要約書類
		 *
		 * @throws IOException 書き込み時の例外
		 */
		public void encode(Map<String, String> map) throws IOException;

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		public void close() throws IOException;
	}
}
