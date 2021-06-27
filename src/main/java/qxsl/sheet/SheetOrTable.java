/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.LinkedList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.StringJoiner;

import qxsl.model.Item;
import qxsl.table.TableManager;

/**
 * 要約書類または交信記録から適切な書式で交信記録を読み取ります。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/05
 */
public final class SheetOrTable {
	private final TableManager tables;
	private final SheetManager sheets;
	private final List<SheetFactory> strips;

	/**
	 * インスタンスを構築します。
	 *
	 *
	 * @see ServiceLoader#load(Class)
	 */
	public SheetOrTable() {
		this(SheetOrTable.class.getClassLoader());
	}

	/**
	 * 指定されたローダを参照するインスタンスを構築します。
	 *
	 *
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public SheetOrTable(ClassLoader cl) {
		this.tables = new TableManager(cl);
		this.sheets = new SheetManager(cl);
		this.strips = new LinkedList<>();
		for(var f: sheets) strips.add(f);
		strips.add(null);
	}

	/**
	 * 指定されたバイト列から交信記録を抽出します。
	 *
	 *
	 * @param binary 要約書類を読み込むバイト列
	 *
	 * @return 抽出された交信記録
	 *
	 * @throws UncheckedIOException 読み込み時の例外
	 */
	public List<Item> unpack(byte[] binary) {
		final var join = new StringJoiner("\n");
		for(var f: strips) try {
			if (f == null) return tables.decode(binary);
			else return tables.decode(f.unpack(binary));
		} catch (Exception ex) {
			join.add(String.format("%s: %s", f, ex));
		}
		final var ms = join.toString();
		final var ex = new IOException(ms);
		throw new UncheckedIOException(ex);
	}

	/**
	 * 指定された文字列から交信記録を抽出します。
	 *
	 *
	 * @param string 要約書類を読み込む文字列
	 *
	 * @return 抽出された交信記録
	 *
	 * @throws UncheckedIOException 読み込み時の例外
	 */
	public List<Item> unpack(String string) {
		final var join = new StringJoiner("\n");
		for(var f: strips) try {
			if (f == null) return tables.decode(string);
			else return tables.decode(f.unpack(string));
		} catch (Exception ex) {
			join.add(String.format("%s: %s", f, ex));
		}
		final var ms = join.toString();
		final var ex = new IOException(ms);
		throw new UncheckedIOException(ex);
	}
}
