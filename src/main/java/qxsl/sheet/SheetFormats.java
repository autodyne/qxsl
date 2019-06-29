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
import java.net.URL;
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
public final class SheetFormats implements Iterable<SheetFormat> {
	private final ServiceLoader<SheetFormat> loader;

	/**
	 * 現在のクラスローダからインスタンス化機構を構築します。
	 */
	public SheetFormats() {
		this(Thread.currentThread().getContextClassLoader());
	}

	/**
	 * 指定のクラスローダからインスタンス化機構を構築します。
	 * 
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public SheetFormats(ClassLoader cl) {
		loader = ServiceLoader.load(SheetFormat.class, cl);
	}

	/**
	 * クラスパスから検出された{@link SheetFormat}を返します。
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public Iterator<SheetFormat> iterator() {
		return loader.iterator();
	}

	/**
	 * 指定された名前を持つ{@link SheetFormat}を検索して返します。
	 * 
	 * 
	 * @param name 属性の名前
	 * @return 対応する書式 存在しない場合null
	 */
	public SheetFormat getFormat(String name) {
		for(SheetFormat fmt: loader) {
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
	 *
	 * @throws IOException 入力時の例外
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
	 * 指定された入力から提出書類を読み取って交信記録を抽出します。
	 * 
	 * 
	 * @param in 提出書類を読み込むストリーム
	 * @return 交信記録の文字列
	 * 
	 * @throws IOException 入出力例外もしくは対応する書式がない場合
	 */
	public String unseal(InputStream in) throws IOException {
		InputStream bin = this.fetch(in);
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		for(SheetFormat format: this) try {
			return format.decode(bin).get(format.getTableKey());
		} catch (IOException ex) {
			pw.printf("%s: %s%n", format, ex.getMessage());
			bin.reset();
		}
		throw new IOException("unsupported format:\n" + sw);
	}

	/**
	 * 指定された入力から提出書類を読み取って交信記録を抽出します。
	 * 
	 * 
	 * @param url 提出書類を読み込むURL
	 * @return 交信記録の文字列
	 * 
	 * @throws IOException 入出力例外もしくは対応する書式がない場合
	 */
	public String unseal(URL url) throws IOException {
		try (InputStream is = url.openStream()) {
			return unseal(is);
		}
	}

	/**
	 * 指定された入力から提出書類を読み取って交信記録を抽出します。
	 * 
	 * 
	 * @param bytes 提出書類を読み込むバイト列
	 * @return 交信記録の文字列
	 * 
	 * @throws IOException 入出力例外もしくは対応する書式がない場合
	 */
	public String unseal(byte[] bytes) throws IOException {
		return unseal(new ByteArrayInputStream(bytes));
	}
}
