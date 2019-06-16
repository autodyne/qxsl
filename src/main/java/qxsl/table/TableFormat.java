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
	 * この書式を適用するファイル名拡張子の不変のリストを返します。
	 * 
	 * @return ファイル名拡張子のリスト
	 */
	public List<String> getExtensions();

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException;

	/**
	 * この書式でエンコードした交信記録を指定したストリームに書き込みます。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException;
}
