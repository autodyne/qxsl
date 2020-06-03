/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.bind;

import java.util.ServiceLoader;
import javax.script.Bindings;
import javax.script.SimpleBindings;

import elva.core.ElvaBool;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.core.ElvaNode;

/**
 * LISP処理系の組み込み関数のスコープを実装します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/01
 */
public final class Forms extends SimpleBindings {
	private final ServiceLoader<ElvaForm> forms;
	private final Bindings outer;

	/**
	 * 組み込み関数のスコープを構築します。
	 *
	 * @param java パッケージのスコープ
	 */
	public Forms(Types java)  {
		this.outer = java;
		this.put("null", null);
		this.put("nil", ElvaList.NIL);
		this.put(ElvaBool.T.toString(), ElvaBool.T);
		this.put(ElvaBool.F.toString(), ElvaBool.F);
		this.forms = ServiceLoader.load(ElvaForm.class);
		for(var form: this.forms) this.preInstall(form);
	}

	/**
	 * 指定された関数をこの環境に登録します。
	 *
	 * @param form 登録する関数
	 */
	private final void preInstall(ElvaForm form) {
		this.put(form.toString(), form);
	}

	/**
	 * 指定された名前に束縛された値を返します。
	 *
	 * @param name 名前
	 * @return 束縛された値
	 */
	@Override
	public final ElvaNode get(Object name) {
		final String key = name.toString();
		if (containsKey(key)) {
			return ElvaNode.wrap(super.get(key));
		} else {
			return ElvaNode.wrap(outer.get(key));
		}
	}
}
