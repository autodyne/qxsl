/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.sheet;

import java.util.Iterator;
import java.util.ServiceLoader;

/**
 * 要約書類の書式を自動的に検出して管理する仕組みです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public final class SheetManager implements Iterable<SheetFactory> {
	private final ServiceLoader<SheetFactory> list;

	/**
	 * インスタンスを構築します。
	 *
	 *
	 * @see ServiceLoader#load(Class)
	 */
	public SheetManager() {
		this(SheetManager.class.getClassLoader());
	}

	/**
	 * 指定されたローダから書式の実装を検索します。
	 *
	 *
	 * @param cl 書式の実装を検出するクラスローダ
	 */
	public SheetManager(ClassLoader cl) {
		this.list = ServiceLoader.load(SheetFactory.class, cl);
	}

	/**
	 * このインスタンスが検出した書式を列挙します。
	 *
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public final Iterator<SheetFactory> iterator() {
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
	public final SheetFactory factory(String name) {
		for(var f: list) if(f.type().equals(name)) return f;
		for(var f: list) if(f.name().equals(name)) return f;
		for(var f: list) for(var ext: f.extensions()) {
			if(ext.equalsIgnoreCase(name)) return f;
		}
		return null;
	}
}
