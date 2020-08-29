/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.*;
import java.util.stream.Collectors;

import qxsl.model.Item;
import qxsl.table.TableFormats;

/**
 * {@link SheetFormat}実装クラスを自動的に検出して管理します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public final class SheetFormats implements Iterable<SheetFormat> {
	private final ServiceLoader<SheetFormat> loader;

	/**
	 * インスタンスを構築します。
	 *
	 * @see ServiceLoader#load(Class)
	 */
	public SheetFormats() {
		this(SheetFormats.class.getClassLoader());
	}

	/**
	 * 指定されたローダを参照するインスタンスを構築します。
	 *
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public SheetFormats(ClassLoader cl) {
		this.loader = ServiceLoader.load(SheetFormat.class, cl);
	}

	/**
	 * このインスタンスが検出した書式を返します。
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public Iterator<SheetFormat> iterator() {
		return loader.iterator();
	}

	/**
	 * 指定された名前を持つ書式の実装を検索します。
	 *
	 *
	 * @param name 属性の名前
	 *
	 * @return 対応する書式 またはnull
	 */
	public SheetFormat forName(String name) {
		for(var f: loader) if(f.getName().equals(name)) return f;
		return null;
	}

	/**
	 * 指定されたリーダから適切な書式で交信記録を読み込みます。
	 *
	 *
	 * @param reader 要約記録を読み込むリーダ
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> unpack(Reader reader) throws IOException {
		try(final var br = new BufferedReader(reader)) {
			return unpack(br.lines().collect(Collectors.joining("\n")));
		}
	}

	/**
	 * 指定された文字列から適切な書式で交信記録を読み込みます。
	 *
	 *
	 * @param string 要約書類を読み込む文字列
	 *
	 * @return 交信記録
	 *
	 * @throws IOException 読み込み時の例外もしくは書式が未知の場合
	 */
	public List<Item> unpack(String string) throws IOException {
		final var err = new StringJoiner(",");
		final var tables = new TableFormats();
		for(var fmt: this) try(var decoder = fmt.decoder(string)) {
			final Map<String, String> map = decoder.decode();
			return tables.decode(map.get(fmt.getTableKey()));
		} catch (IOException ex) {
			err.add(fmt.toString());
		}
		throw new IOException("none of ".concat(err.toString()));
	}
}
