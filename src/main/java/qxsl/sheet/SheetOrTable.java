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
	private final List<Format> strips;

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
		for(var f: sheets) strips.add(new Format(f));
		strips.add(new Format(new RawDataFactory()));
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
			return tables.decode(f.unpack(binary));
		} catch (Exception ex) {
			join.add(f.fmt.name().concat(":"));
			join.add(ex.getMessage()).add("");
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
			return tables.decode(f.unpack(string));
		} catch (Exception ex) {
			join.add(f.fmt.name().concat(":"));
			join.add(ex.getMessage()).add("");
		}
		final var ms = join.toString();
		final var ex = new IOException(ms);
		throw new UncheckedIOException(ex);
	}

	/**
	 * 剥き出しの交信記録を表す便宜的な要約書類の書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2021/06/28
	 */
	private final class RawDataFactory extends BasicFactory {
		/**
		 * 書式を構築します。
		 */
		public RawDataFactory() {
			super("raw");
		}
	}

	/**
	 * 指定された要約書類の書式により交信記録を抽出します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2021/06/28
	 */
	private static final class Format {
		private final SheetFactory fmt;

		/**
		 * 書式を指定します。
		 *
		 * @param fmt 書式
		 */
		public Format(SheetFactory fmt) {
			this.fmt = fmt;
		}

		/**
		 * 指定された要約書類を読み取り交信記録を抽出します。
		 *
		 *
		 * @param data 要約書類
		 *
		 * @return 抽出された交信記録
		 *
		 * @throws IOException 読み込み時の例外
		 *
		 * @throws UnsupportedOperationException 未実装の場合
		 */
		private String unpack(String data) throws IOException {
			if(fmt instanceof RawDataFactory) return data;
			try(var decoder = fmt.decoder(data).decode()) {
				return decoder.getString(fmt.getTableKey());
			}
		}

		/**
		 * 指定された要約書類を読み取り交信記録を抽出します。
		 *
		 *
		 * @param data 要約書類
		 *
		 * @return 抽出された交信記録
		 *
		 * @throws IOException 読み込み時の例外
		 *
		 * @throws UnsupportedOperationException 未実装の場合
		 */
		private byte[] unpack(byte[] data) throws IOException {
			if(fmt instanceof RawDataFactory) return data;
			try(var decoder = fmt.decoder(data).decode()) {
				return decoder.getBinary(fmt.getTableKey());
			}
		}
	}
}
