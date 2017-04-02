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
	 * スレッドの{@link ClassLoader}を指定して機構を構築します。
	 */
	public Tables() {
		this(Thread.currentThread().getContextClassLoader());
	}

	/**
	 * 指定された{@link ClassLoader}から検索する機構を構築します。
	 * 
	 * @param cl クラスローダー
	 */
	public Tables(ClassLoader cl) {
		loader = ServiceLoader.load(TableFormat.class, cl);
	}

	@Override
	public final Iterator<TableFormat> iterator() {
		return loader.iterator();
	}

	/**
	 * 指定された名前を持つ{@link TableFormat}を検索して返します。
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
	 * フォーマットを自動判別して交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * 
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		ByteArrayOutputStream bout = new ByteArrayOutputStream();
		final byte[] buffer = new byte[1024];
		int len = 0;
		try (InputStream inputStream = in) {
			while((len = in.read(buffer)) > 0) {
				bout.write(buffer, 0, len);
			}
		}
		final byte[] mem = bout.toByteArray();
		ByteArrayInputStream bin = new ByteArrayInputStream(mem);
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		for(TableFormat format: loader) try {
			return format.decode(bin);
		} catch(IOException ex) {
			pw.print(format.getName());
			pw.print(": ");
			pw.println(ex.getMessage());
			bin.reset();
		}
		throw new IOException("unsupported format:\n" + sw);
	}

	/**
	 * フォーマットを自動判別して交信記録を読み込みます。
	 * 
	 * @param url 交信記録を読み込むURL
	 * @return 交信記録
	 * 
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(java.net.URL url) throws IOException {
		return decode(url.openStream());
	}
}
