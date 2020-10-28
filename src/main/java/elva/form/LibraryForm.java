/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.ListBase;
import elva.lang.NameNode;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.ruler.Library;

/**
 * creates and returns a library object.
 * <pre>
 * (library name)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/26
 */
@Name("library")
@Args(min = 1, max = 1)
public final class LibraryForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		return new LibraryImpl(args, eval);
	}
}

/**
 * LISP処理系の内部におけるコンテストの規約の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/20
 */
final class LibraryImpl extends Library {
	private final String name;
	private final ElvaEval eval;

	/**
	 * 指定された規約定義と評価器で規約を構築します。
	 *
	 *
	 * @param rule 規約
	 * @param eval 評価器
	 */
	public LibraryImpl(ListBase rule, ElvaEval eval) {
		this.name = eval.apply(rule.head()).text();
		this.eval = eval;
	}

	/**
	 * ライブラリの名前を返します。
	 *
	 *
	 * @return 名前
	 */
	@Override
	public final String getName() {
		return name;
	}

	/**
	 * このライブラリが参照する変数を実行します。
	 *
	 *
	 * @param name 変数の名前
	 *
	 * @return 変数の値
	 *
	 * @since 2020/09/27
	 */
	@Override
	public final Object get(String name) {
		return eval.apply(new NameNode(name)).value();
	}

	/**
	 * このライブラリが参照する関数を実行します。
	 *
	 *
	 * @param name 関数の名前
	 * @param args 関数の引数
	 *
	 * @return 関数の値
	 *
	 * @since 2020/03/09
	 */
	@Override
	public final Object invoke(String name, Object...args) {
		return eval.apply(new NameNode(name).form(args)).value();
	}
}
