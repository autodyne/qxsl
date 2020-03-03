/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.table;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;
import java.util.StringJoiner;
import java.util.stream.Collectors;
import qxsl.model.Item;

/**
 * {@link TableFormat}実装クラスを自動的に検出して管理します。
 * 
 * 
 * @author 無線部開発班
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
		return decode(is.readAllBytes());
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
		final var bis = new ByteArrayInputStream(b);
		final var err = new StringJoiner(",");
		for(TableFormat fmt: this) {
			try(var decoder = fmt.decoder(bis)) {
				return decoder.decode();
			} catch (IOException ex) {
				bis.reset();
				err.add(fmt.toString());
			}
		}
		throw new IOException("none of ".concat(err.toString()));
	}

	/**
	 * 指定されたリーダから適切な書式で交信記録を読み込みます。
	 * 
	 * @param reader 交信記録を読み込むリーダ
	 * @return 交信記録
	 * 
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> decode(Reader reader) throws IOException {
		try(BufferedReader br = new BufferedReader(reader)) {
			return decode(br.lines().collect(Collectors.joining("\n")));
		}
	}

	/**
	 * 指定された文字列から適切な書式で交信記録を読み込みます。
	 * 
	 * @param string 交信記録を読み込む文字列
	 * @return 交信記録
	 * 
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> decode(String string) throws IOException {
		final var err = new StringJoiner(",");
		for(TableFormat fmt: this) {
			try(var decoder = fmt.decoder(new StringReader(string))) {
				return decoder.decode();
			} catch (IOException ex) {
				err.add(fmt.toString());
			} catch (UnsupportedOperationException ex) {}
		}
		throw new IOException("none of ".concat(err.toString()));
	}
}
