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
import java.time.ZoneId;
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
	 * 指定されたストリームの内容がこの書式に従う交信記録であるか検証します。
	 *
	 * @param strm 交信記録を読み込むストリーム
	 * @return 交信記録が読み込める場合に限り真
	 */
	public default boolean validate(InputStream strm) {
		try {
			decode(strm);
			return true;
		} catch (IOException ex) {
			return false;
		}
	}

	/**
	 * 指定されたストリームをこの書式でデコードして、交信記録を読み込みます。
	 * これは{@link #decode decode(strm, ZoneId#systemDefault())}と同等です。
	 * 
	 * @param strm 交信記録を読み込むストリーム
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外
	 */
	public default List<Item> decode(InputStream strm) throws IOException {
		return this.decode(strm, ZoneId.systemDefault());
	}

	/**
	 * 指定されたストリームをこの書式でデコードして、交信記録を読み込みます。
	 * 
	 * @param strm 交信記録を読み込むストリーム
	 * @param zone 交信記録のタイムゾーン
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外
	 */
	public List<Item> decode(InputStream strm, ZoneId zone) throws IOException;

	/**
	 * この書式でエンコードした交信記録を指定されたストリームに出力します。
	 * QXML以外の書式では交信記録の一部の属性が出力されない場合があります。
	 * 
	 * @param strm 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 *
	 * @throws IOException 書き込み時の例外
	 */
	public void encode(OutputStream strm, List<Item> items) throws IOException;
}
