/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import qxsl.model.Item;

/**
 * 交信記録を永続化する書式はこのクラスを継承します。
 * 
 * 
 * @author Journal of Hamradio Informatics
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
	 * 指定されたストリームの内容を検証します。
	 *
	 * @param is 内容を検証するストリーム
	 * @return この書式に従う場合に限り真
	 */
	public default boolean verify(InputStream is) {
		try {
			decoder(is).decode();
			return true;
		} catch (IOException ex) {
			return false;
		}
	}

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
	 * @param os 交信記録を書き出すストリーム
	 * @return エンコーダ
	 */
	public abstract TableEncoder encoder(OutputStream os);

	/**
	 * 永続化された交信記録を読み込むためのデコーダです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
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
	}

	/**
	 * 交信記録を書き出して永続化するためのエンコーダです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
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
	}
}
