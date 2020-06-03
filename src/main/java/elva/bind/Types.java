/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.bind;

import javax.script.SimpleBindings;

import elva.core.ElvaNode;
import elva.core.ElvaType;
import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系でパッケージを検索する仮想的なスコープの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/02
 */
public final class Types extends SimpleBindings {
	private final ClassLoader loader;

	/**
	 * パッケージのスコープを構築します。
	 *
	 * @param loader クラスローダ
	 */
	public Types(ClassLoader loader) {
		this.loader = loader;
		importClass(int.class);
		importClass(long.class);
		importClass(char.class);
		importClass(byte.class);
		importClass(short.class);
		importClass(float.class);
		importClass(double.class);
		importClass(boolean.class);
	}

	/**
	 * 指定された型を短縮名で参照可能にします。
	 *
	 * @param type 型
	 */
	public final void importClass(Class<?> type) {
		put(type.getSimpleName(), new ElvaType(type));
	}

	/**
	 * 指定された名前に束縛された値を返します。
	 *
	 * @param name 名前
	 * @return 束縛された値
	 */
	@Override
	public final ElvaNode get(Object name) {
		final var text = name.toString();
		final var data = super.get(text);
		try {
			if(data != null) return ElvaNode.wrap(data);
			return new ElvaType(loader.loadClass(text));
		} catch (ClassNotFoundException ex) {
			final var msg = "name '%s is not defined";
			throw new ElvaRuntimeException(msg, name);
		}
	}
}
