/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;
import java.util.StringJoiner;
import qxsl.model.Item;

/**
 * {@link TableFormat}実装クラスを自動的に検出して管理します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/02/25
 *
 */
public final class TableFormats implements Iterable<TableFormat> {
	private final ServiceLoader<TableFormat> loader;

	/**
	 * インスタンスを構築します。
	 *
	 * @see ServiceLoader#load(Class)
	 */
	public TableFormats() {
		loader = ServiceLoader.load(TableFormat.class);
	}

	/**
	 * 指定されたローダを参照するインスタンスを構築します。
	 * 
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public TableFormats(ClassLoader cl) {
		loader = ServiceLoader.load(TableFormat.class, cl);
	}

	/**
	 * このインスタンスが検出した書式を返します。
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public Iterator<TableFormat> iterator() {
		return loader.iterator();
	}

	/**
	 * 指定された名前を持つ書式の実装を検索します。
	 * 
	 * @param name 属性の名前
	 * @return 対応する書式 存在しない場合null
	 */
	public TableFormat forName(String name) {
		for(TableFormat fmt: loader) {
			if(fmt.getName().equals(name)) return fmt;
		}
		return null;
	}

	/**
	 * 指定されたストリームから適切な書式で交信記録を読み込みます。
	 * 
	 * @param is 交信記録を読み込むストリーム
	 * @return 交信記録
	 * 
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> decode(InputStream is) throws IOException {
		InputStream bis = new ByteArrayInputStream(is.readAllBytes());
		final StringJoiner err = new StringJoiner(", ");
		for(TableFormat format: this) try {
			return format.decoder(bis).decode();
		} catch (IOException ex) {
			bis.reset();
			err.add(format.toString());
		}
		throw new IOException("none of ".concat(err.toString()));
	}

	/**
	 * 指定されたバイト配列から適切な書式で交信記録を読み込みます。
	 * 
	 * @param b 交信記録を読み込むバイト配列
	 * @return 交信記録
	 * 
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> decode(final byte[] b) throws IOException {
		return decode(new ByteArrayInputStream(b));
	}
}
