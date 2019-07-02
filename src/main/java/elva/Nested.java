/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import javax.script.Bindings;
import javax.script.SimpleBindings;

/**
 * LISP処理系の識別子を捕捉する静的スコープの実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/18
 */
public final class Nested extends SimpleBindings {
	private final Bindings outer;

	/**
	 * 内容と上位のスコープを指定してスコープを構築します。
	 *
	 * @param outer 外側のスコープ
	 * @param binds スコープの内容
	 */
	public Nested(Bindings binds, Bindings outer)  {
		if(binds != null) putAll(binds);
		this.outer = outer;
	}

	/**
	 * 指定された名前に束縛された値を返します。
	 *
	 * @param name 名前
	 * @return 束縛された値
	 */
	@Override
	public final Object get(Object name) {
		final String key = name.toString();
		if(containsKey(key)) {
			return super.get(key);
		} else if(outer != null) {
			return outer.get(key);
		}
		final String msg = "unknown symbol '%s'";
		throw new ElvaRuntimeException(msg, name);
	}

	/**
	 * 指定された関数をこの環境に登録します。
	 *
	 * @param func 登録する関数
	 */
	public final void put(Function func) {
		this.put(func.toString(), func);
	}

	/**
	 * 指定された値を名前で束縛します。
	 *
	 * @param name 名前
	 * @param target 値
	 */
	public final void put(Symbol name, Object target) {
		this.put(name.toString(), target);
	}
}
