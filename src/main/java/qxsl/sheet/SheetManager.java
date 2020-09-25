/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.IOException;
import java.util.Iterator;
import java.util.ServiceLoader;
import java.util.StringJoiner;

/**
 * {@link SheetFactory}実装クラスを自動的に検出して管理します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public final class SheetManager implements Iterable<SheetFactory> {
	private final ServiceLoader<SheetFactory> list;

	/**
	 * インスタンスを構築します。
	 *
	 *
	 * @see ServiceLoader#load(Class)
	 */
	public SheetManager() {
		this(SheetManager.class.getClassLoader());
	}

	/**
	 * 指定されたローダを参照するインスタンスを構築します。
	 *
	 *
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public SheetManager(ClassLoader cl) {
		this.list = ServiceLoader.load(SheetFactory.class, cl);
	}

	/**
	 * このインスタンスが検出した書式を返します。
	 *
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public Iterator<SheetFactory> iterator() {
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
	public SheetFactory forName(String name) {
		for(var f: list) if(f.getName().equals(name)) return f;
		return null;
	}

	/**
	 * 指定されたバイト列から要約書類を読み込み交信記録を抽出します。
	 *
	 *
	 * @param binary 要約書類を読み込むバイト列
	 *
	 * @return 抽出された交信記録
	 *
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public byte[] unpack(byte[] binary) throws IOException {
		final var join = new StringJoiner("\n");
		for(var f: this) try {
			return f.unpack(binary);
		} catch (Exception ex) {
			join.add(String.format("-(%s):", f));
			join.add(ex.toString());
		}
		throw new IOException(join.toString());
	}

	/**
	 * 指定された文字列から要約書類を読み込み交信記録を抽出します。
	 *
	 *
	 * @param string 要約書類を読み込む文字列
	 *
	 * @return 抽出された交信記録
	 *
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public byte[] unpack(String string) throws IOException {
		final var join = new StringJoiner("\n");
		for(var f: this) try {
			return f.unpack(string);
		} catch (Exception ex) {
			join.add(String.format("-(%s):", f));
			join.add(ex.toString());
		}
		throw new IOException(join.toString());
	}
}
