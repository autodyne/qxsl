/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import javax.script.Bindings;
import javax.script.SimpleBindings;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系の識別子を捕捉する静的スコープの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class ScopeMap extends SimpleBindings {
	private final ClassLoader types;
	private final Bindings outer;

	/**
	 * 最も外側のスコープを構築します。
	 *
	 * @param loader クラスローダ
	 */
	public ScopeMap(ClassLoader loader) {
		this.types = loader;
		this.outer = null;
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
	 * 指定された構造のスコープを構築します。
	 *
	 * @param outer 外側のスコープ
	 */
	private ScopeMap(Bindings outer)  {
		this.outer = outer;
		this.types = null;
	}

	/**
	 * スコープの内側にスコープを構築します。
	 *
	 * @return 内側のスコープ
	 */
	public final ScopeMap fork() {
		return new ScopeMap(this);
	}

	/**
	 * 指定された型を短縮名で参照可能にします。
	 *
	 * @param type 型
	 */
	public final void importClass(Class<?> type) {
		put(type.getSimpleName(), new TypeNode(type));
	}

	/**
	 * 指定された内容をこのスコープに追加します。
	 *
	 *
	 * @param binds 追加する内容
	 *
	 * @return このスコープ
	 */
	public final ScopeMap merge(Bindings binds) {
		if(binds != null) putAll(binds);
		return this;
	}

	/**
	 * 指定された変数をこの環境に登録します。
	 * 変数の名前は値の文字列による表現です。
	 *
	 * @param sexp 登録する変数の値
	 */
	public void put(NodeBase sexp) {
		put(sexp.toString(), sexp);
	}

	/**
	 * 指定された名前を指定された値で束縛します。
	 *
	 *
	 * @param name 登録する変数の名前
	 * @param sexp 登録する変数の値
	 */
	public void put(NodeBase name, NodeBase sexp) {
		put(name.toString(), sexp);
	}

	/**
	 * 指定された仮引数と実引数の組を束縛します。
	 *
	 *
	 * @param pars 仮引数のリスト
	 * @param args 実引数のリスト
	 */
	public void put(ListBase pars, ListBase args) {
		final var p = pars.iterator();
		final var a = args.iterator();
		while(p.hasNext()) put(p.next(), a.next());
	}

	/**
	 * 指定された名前を束縛する値を返します。
	 *
	 *
	 * @param key 変数の名前
	 *
	 * @return 変数の値
	 *
	 * @throws ElvaRuntimeException 未束縛の場合
	 */
	@Override
	public final NodeBase get(Object key) {
		final var name = key.toString();
		if(containsKey(name)) {
			return NodeBase.wrap(super.get(name));
		} else if(outer != null) {
			return NodeBase.wrap(outer.get(name));
		} else try {
			return new TypeNode(types.loadClass(name));
		} catch (ClassNotFoundException ex) {
			final var text = "symbol '%s is not bound";
			throw new ElvaRuntimeException(text, name);
		}
	}
}
