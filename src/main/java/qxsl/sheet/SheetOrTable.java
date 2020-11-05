/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.UncheckedIOException;
import java.util.List;
import java.util.ServiceLoader;

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
	private final SheetManager sheets;
	private final TableManager tables;

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
		this.sheets = new SheetManager(cl);
		this.tables = new TableManager(cl);
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
		try {
			return tables.decode(sheets.unpack(binary));
		} catch (UncheckedIOException ex) {
			return tables.decode(binary);
		}
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
		try {
			return tables.decode(sheets.unpack(string));
		} catch (UncheckedIOException ex) {
			return tables.decode(string);
		}
	}
}
