/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.sheet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.ServiceLoader;

/**
 * {@link SheetFormat}クラスの自動検出及びインスタンス化機構を実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/03/11
 *
 */
public final class Sheets implements Iterable<SheetFormat> {
	private final ServiceLoader<SheetFormat> loader;

	/**
	 * スレッドの{@link ClassLoader}を指定して機構を構築します。
	 */
	public Sheets() {
		this(Thread.currentThread().getContextClassLoader());
	}

	/**
	 * 指定された{@link ClassLoader}から検索する機構を構築します。
	 * 
	 * @param cl クラスローダー
	 */
	public Sheets(ClassLoader cl) {
		loader = ServiceLoader.load(SheetFormat.class, cl);
	}

	@Override
	public final Iterator<SheetFormat> iterator() {
		return loader.iterator();
	}

	/**
	 * 指定された名前を持つ{@link SheetFormat}を検索して返します。
	 * 
	 * @param name 属性の名前
	 * @return 対応するフォーマット 存在しない場合null
	 */
	public SheetFormat getFormat(String name) {
		for(SheetFormat fmt: loader) {
			if(fmt.getName().equals(name)) return fmt;
		}
		return null;
	}

	/**
	 * フォーマットを自動判別して提出書類を読み込みます。
	 * 
	 * @param in 提出書類を読み込むストリーム
	 * @return 提出書類 提出書類がなければnull
	 * 
	 * @throws IOException 入出力時の例外
	 */
	public Map<String, String> decode(InputStream in) throws IOException {
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
		for(SheetFormat format: loader) try {
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
	 * フォーマットを自動判別して提出書類を読み込みます。
	 * 
	 * @param url 提出書類を読み込むURL
	 * @return 提出書類
	 * 
	 * @throws IOException 入出力時の例外
	 */
	public Map<String, String> decode(java.net.URL url) throws IOException {
		try (InputStream is = url.openStream()) {
			return decode(is);
		}
	}

	/**
	 * フォーマットを自動判別して提出書類を読み込みます。
	 * 
	 * @param bytes 提出書類を読み込むバイト列
	 * @return 提出書類
	 * 
	 * @throws IOException 入出力時の例外
	 */
	public Map<String, String> decode(byte[] bytes) throws IOException {
		return decode(new ByteArrayInputStream(bytes));
	}
}
