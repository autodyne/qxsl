/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import javax.script.Bindings;
import javax.script.SimpleBindings;

import elva.Elva.ElvaRuntimeException;

/**
 * LISP処理系の識別子を捕捉する静的スコープの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class Nest extends SimpleBindings {
	private final Bindings outer;

	/**
	 * 局所変数のスコープを構築します。
	 *
	 * @param outer 外側のスコープ
	 */
	public Nest(Bindings outer)  {
		this.outer = outer;
	}

	/**
	 * 指定された名前に束縛された値を返します。
	 *
	 * @param name 名前
	 * @return 束縛された値
	 */
	@Override
	public final Sexp get(Object name) {
		final String key = name.toString();
		if (containsKey(key)) {
			return Sexp.wrap(super.get(key));
		} else if(outer != null) {
			return Sexp.wrap(outer.get(key));
		}
		final String msg = "unknown symbol '%s'";
		throw new ElvaRuntimeException(msg, name);
	}

	/**
	 * 指定された関数をこの環境に登録します。
	 *
	 * @param form 登録する関数
	 */
	public final void put(Form form) {
		this.put(form.toString(), form);
	}

	/**
	 * 指定された値を名前で束縛します。
	 *
	 * @param name 名前
	 * @param sexp 値
	 */
	public final void put(Name name, Sexp sexp) {
		this.put(name.toString(), sexp);
	}

	/**
	 * 指定された環境をこの環境に統合します。
	 *
	 * @param nest 環境
	 * @return この環境
	 */
	public final Nest merge(Bindings nest) {
		if(nest != null) this.putAll(nest);
		return this;
	}
}
