/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;
import java.util.StringJoiner;

import qxsl.model.Item;

/**
 * {@link TableFactory}実装クラスを自動的に検出して管理します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/25
 */
public final class TableManager implements Iterable<TableFactory> {
	private final ServiceLoader<TableFactory> list;

	/**
	 * インスタンスを構築します。
	 *
	 *
	 * @see ServiceLoader#load(Class)
	 */
	public TableManager() {
		this(TableManager.class.getClassLoader());
	}

	/**
	 * 指定されたローダを参照するインスタンスを構築します。
	 *
	 *
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public TableManager(ClassLoader cl) {
		this.list = ServiceLoader.load(TableFactory.class, cl);
	}

	/**
	 * このインスタンスが検出した書式を返します。
	 *
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public Iterator<TableFactory> iterator() {
		return list.iterator();
	}

	/**
	 * 指定された名前を持つ書式の実装を検索します。
	 *
	 *
	 * @param name 属性の名前
	 *
	 * @return 対応する書式 またはnull
	 */
	public TableFactory forName(String name) {
		for(var f: list) if(f.getName().equals(name)) return f;
		return null;
	}

	/**
	 * 指定された入力から適切な書式で交信記録を読み込みます。
	 *
	 *
	 * @param in 交信記録を読み込む入力
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> decode(InputStream in) throws IOException {
		return decode(in.readAllBytes());
	}

	/**
	 * 指定されたバイト列から適切な書式で交信記録を読み込みます。
	 *
	 *
	 * @param binary 交信記録を読み込むバイト列
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> decode(byte[] binary) throws IOException {
		final var join = new StringJoiner("\n");
		for(var f: this) try {
			return f.decode(binary);
		} catch (Exception ex) {
			join.add(String.format("-(%s):", f));
			join.add(ex.toString());
		}
		throw new IOException(join.toString());
	}

	/**
	 * 指定された文字列から適切な書式で交信記録を読み込みます。
	 *
	 *
	 * @param string 交信記録を読み込む文字列
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> decode(String string) throws IOException {
		final var join = new StringJoiner("\n");
		for(var f: this) try {
			return f.decode(string);
		} catch (Exception ex) {
			join.add(String.format("-(%s):", f));
			join.add(ex.toString());
		}
		throw new IOException(join.toString());
	}
}
