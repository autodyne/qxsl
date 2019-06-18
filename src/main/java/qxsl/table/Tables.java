/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;
import qxsl.model.Item;

/**
 * {@link TableFormat}クラスの自動検出及びインスタンス化機構を実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/02/25
 *
 */
public final class Tables implements Iterable<TableFormat> {
	private final ServiceLoader<TableFormat> loader;

	/**
	 * 現在の{@link ClassLoader}から書式を検索します。
	 */
	public Tables() {
		this(Thread.currentThread().getContextClassLoader());
	}

	/**
	 * 指定の{@link ClassLoader}から書式を検索します。
	 * 
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public Tables(ClassLoader cl) {
		loader = ServiceLoader.load(TableFormat.class, cl);
	}

	/**
	 * クラスパスから検出された{@link TableFormat}を返します。
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public Iterator<TableFormat> iterator() {
		return loader.iterator();
	}

	/**
	 * 指定された名前を持つ{@link TableFormat}を検索して返します。
	 * 
	 * 
	 * @param name 属性の名前
	 * @return 対応するフォーマット 存在しない場合null
	 */
	public TableFormat getFormat(String name) {
		for(TableFormat fmt: loader) {
			if(fmt.getName().equals(name)) return fmt;
		}
		return null;
	}

	/**
	 * 指定されたストリームを主記憶に読み込みます。
	 * 
	 * 
	 * @param in 内容を読み込むストリーム
	 * @return 内容をコピーしたストリーム
	 */
	private InputStream fetch(InputStream in) throws IOException {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		final byte[] buffer = new byte[1024];
		int len = 0;
		try (InputStream inputStream = in) {
			while((len = in.read(buffer)) > 0) out.write(buffer, 0, len);
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

	/**
	 * 指定された入力について交信記録のフォーマットを自動判別します。
	 * 
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録の書式 対応する書式がない場合はnull
	 * 
	 * @throws IOException 入出力時の例外
	 */
	public TableFormat detect(InputStream in) throws IOException {
		InputStream bin = this.fetch(in);
		for(TableFormat format: this) try {
			format.decode(bin);
			return format;
		} catch (IOException ex) {
			bin.reset();
		}
		return null;
	}

	/**
	 * 指定された入力について提出書類のフォーマットを自動判別します。
	 * 
	 * 
	 * @param url 提出書類を読み込むURL
	 * @return 提出書類の書式 対応する書式がない場合はnull
	 * 
	 * @throws IOException 入出力時の例外
	 */
	public TableFormat detect(URL url) throws IOException {
		try (InputStream is = url.openStream()) {
			return detect(is);
		}
	}

	/**
	 * 指定された入力について提出書類のフォーマットを自動判別します。
	 * 
	 * 
	 * @param bytes 提出書類を読み込むバイト列
	 * @return 提出書類の書式 対応する書式がない場合はnull
	 * 
	 * @throws IOException 入出力時の例外
	 */
	public TableFormat detect(byte[] bytes) throws IOException {
		return detect(new ByteArrayInputStream(bytes));
	}

	/**
	 * 指定された入力からフォーマットを自動判別して交信記録を読み込みます。
	 * 
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * 
	 * @throws IOException 入出力例外もしくは対応する書式がない場合
	 */
	public List<Item> decode(InputStream in) throws IOException {
		InputStream bin = this.fetch(in);
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		for(TableFormat format: this) try {
			return format.decode(bin);
		} catch (IOException ex) {
			pw.printf("%s: %s%n", format, ex.getMessage());
			bin.reset();
		}
		throw new IOException("unsupported format:\n" + sw);
	}

	/**
	 * 指定された入力からフォーマットを自動判別して交信記録を読み込みます。
	 * 
	 * 
	 * @param url 交信記録を読み込むURL
	 * @return 交信記録
	 * 
	 * @throws IOException 入出力例外もしくは対応する書式がない場合
	 */
	public List<Item> decode(java.net.URL url) throws IOException {
		try (InputStream is = url.openStream()) {
			return decode(is);
		}
	}

	/**
	 * 指定された入力からフォーマットを自動判別して交信記録を読み込みます。
	 * 
	 * 
	 * @param bytes 交信記録を読み込むバイト列
	 * @return 交信記録
	 * 
	 * @throws IOException 入出力例外もしくは対応する書式がない場合
	 */
	public List<Item> decode(byte[] bytes) throws IOException {
		return decode(new ByteArrayInputStream(bytes));
	}
}
