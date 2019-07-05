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
import java.io.OutputStream;
import java.net.URL;
import java.time.ZoneId;
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
public final class TableFormats implements Iterable<TableFormat> {
	private final ServiceLoader<TableFormat> loader;

	/**
	 * 現在のクラスローダからインスタンス化機構を構築します。
	 */
	public TableFormats() {
		this(Thread.currentThread().getContextClassLoader());
	}

	/**
	 * 指定のクラスローダからインスタンス化機構を構築します。
	 * 
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public TableFormats(ClassLoader cl) {
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
	 * @return 対応する書式 存在しない場合null
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
	 * 指定されたストリームから書式を自動判別して交信記録を読み込みます。
	 * これは{@link #decode decode(strm, ZoneId#systemDefault())}と同等です。
	 * 
	 * 
	 * @param strm 交信記録を読み込むストリーム
	 * @param zone 交信記録のタイムゾーン
	 * @return 交信記録
	 * 
	 * @throws IOException 入出力例外もしくは対応する書式がない場合
	 */
	public List<Item> decode(InputStream strm) throws IOException {
		return this.decode(strm, ZoneId.systemDefault());
	}

	/**
	 * 指定されたストリームから書式を自動判別して交信記録を読み込みます。
	 * 
	 * 
	 * @param strm 交信記録を読み込むストリーム
	 * @param zone 交信記録のタイムゾーン
	 * @return 交信記録
	 * 
	 * @throws IOException 読み込み時の例外もしくは対応する書式がない場合
	 */
	public List<Item> decode(InputStream strm, ZoneId zone) throws IOException {
		InputStream bin = this.fetch(strm);
		for(TableFormat format: this) {
			if(format.validate(bin)) {
				bin.reset();
				return format.decode(bin, zone);
			} else bin.reset();
		}
		throw new IOException("unsupported format");
	}

	/**
	 * 指定されたストリームに書式をQXMLに設定して交信記録を書き込みます。
	 * 
	 * 
	 * @param strm 交信記録を書き込むストリーム
	 * @param items 交信記録
	 * 
	 * @throws IOException 書き込み時の例外
	 */
	public void encode(OutputStream strm, List<Item> items) throws IOException {
		this.getFormat("qxml").encode(strm, items);
	}
}
