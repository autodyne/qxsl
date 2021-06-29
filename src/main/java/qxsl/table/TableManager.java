/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;
import java.util.StringJoiner;

import qxsl.model.Item;

/**
 * 交信記録の書式をクラスパスから自動的に検出して管理します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/02/25
 */
public final class TableManager implements Iterable<TableFactory> {
	private final ServiceLoader<TableFactory> list;

	/**
	 * インスタンスを構築します。
	 *
	 *
	 * @see ServiceLoader#load(Class)
	 */
	public TableManager() {
		this(TableManager.class.getClassLoader());
	}

	/**
	 * 指定されたローダから書式の実装を検索します。
	 *
	 *
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public TableManager(ClassLoader cl) {
		this.list = ServiceLoader.load(TableFactory.class, cl);
	}

	/**
	 * このインスタンスが検出した書式を列挙します。
	 *
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public final Iterator<TableFactory> iterator() {
		return list.iterator();
	}

	/**
	 * 指定された名前もしくはラベルを持つ書式の実装を検索します。
	 *
	 *
	 * @param name 属性の名前
	 *
	 * @return 対応する書式 またはnull
	 */
	public final TableFactory factory(String name) {
		for(var f: list) if(f.type().equals(name)) return f;
		for(var f: list) if(f.name().equals(name)) return f;
		for(var f: list) for(var ext: f.extensions()) {
			if(ext.equalsIgnoreCase(name)) return f;
		}
		return null;
	}

	/**
	 * 指定されたバイト列から適切な書式で交信記録を読み込みます。
	 *
	 *
	 * @param binary 交信記録を読み込むバイト列
	 *
	 * @return 交信記録
	 *
	 * @throws UncheckedIOException 読み込み時の例外
	 */
	public final List<Item> decode(byte[] binary) {
		final var join = new StringJoiner("\n");
		for(var f: this) try {
			return f.decode(binary);
		} catch (Exception ex) {
			join.add(cause(f, ex));
		}
		final var ms = join.toString();
		final var ex = new IOException(ms);
		throw new UncheckedIOException(ex);
	}

	/**
	 * 指定された文字列から適切な書式で交信記録を読み込みます。
	 *
	 *
	 * @param string 交信記録を読み込む文字列
	 *
	 * @return 交信記録
	 *
	 * @throws UncheckedIOException 読み込み時の例外
	 */
	public final List<Item> decode(String string) {
		final var join = new StringJoiner("\n");
		for(var f: this) try {
			return f.decode(string);
		} catch (Exception ex) {
			join.add(cause(f, ex));
		}
		final var ms = join.toString();
		final var ex = new IOException(ms);
		throw new UncheckedIOException(ex);
	}

	/**
	 * 指定された例外またはエラーの最初の原因を返します。
	 *
	 *
	 * @param tf 書式
	 * @param ex 例外またはエラー
	 *
	 * @return 最初の原因の文字列
	 */
	private final String cause(TableFactory tf, Throwable ex) {
		while(ex.getCause() != null) ex = ex.getCause();
		final var m = (ex != null)? ex.getMessage(): "";
		return String.format(" [%s]: %s", tf.name(), m);
	}

	/**
	 * 指定された交信記録をQXML書式のバイト列に書き出します。
	 *
	 *
	 * @param list 交信記録
	 *
	 * @return バイト列
	 *
	 * @throws UncheckedIOException 書き込み時の例外
	 *
	 * @since 2020/11/16
	 */
	public final byte[] encode(List<Item> list) {
		return factory("qxml").encode(list);
	}

	/**
	 * 指定された交信記録をQXML書式のバイト列に書き出します。
	 *
	 *
	 * @param sequence 交信記録
	 *
	 * @return バイト列
	 *
	 * @throws UncheckedIOException 書き込み時の例外
	 *
	 * @since 2020/11/16
	 */
	public final byte[] encode(Item...sequence) {
		return encode(List.of(sequence));
	}
}
