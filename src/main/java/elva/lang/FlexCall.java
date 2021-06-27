/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系からメソッドを参照する演算子の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
public final class FlexCall extends FormBase {
	private final NameNode name;
	private final TypeNode type;

	/**
	 * 指定された名前のメソッドを参照します。
	 *
	 *
	 * @param name メソッドの名前
	 */
	public FlexCall(NameNode name) {
		this(name, null);
	}

	/**
	 * 指定された名前と型のメソッドを参照します。
	 *
	 *
	 * @param name メソッドの名前
	 * @param type メソッドを定義した型
	 */
	public FlexCall(NameNode name, TypeNode type) {
		this.name = name;
		this.type = type;
	}

	/**
	 * この演算子の実体を返します。
	 *
	 *
	 * @return 演算子の実体
	 */
	@Override
	public final FlexCall value() {
		return this;
	}

	/**
	 * この演算子が可変長引数のメソッドか確認します。
	 *
	 *
	 * @return 常に偽
	 */
	@Override
	public final boolean isVarArgs() {
		return false;
	}

	/**
	 * このメソッドが取る引数の最小の個数を返します。
	 *
	 *
	 * @return 最小限の引数の個数
	 */
	@Override
	public final int getMinimumArgumentLength() {
		return type != null? 0: 1;
	}

	/**
	 * このメソッドが取る引数の最大の個数を返します。
	 *
	 *
	 * @return 最大限の引数の個数
	 */
	@Override
	public final int getMaximumArgumentLength() {
		return MAX;
	}

	/**
	 * このメソッドを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return String.format("(method! '%s)", name);
	}

	/**
	 * 指定された対象と実引数でメソッドを実行します。
	 *
	 *
	 * @param type メソッドを定義した型
	 * @param args オブジェクトと実引数の配列
	 *
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	private final Object invoke(TypeNode type, ListBase args) {
		for(var method: type.getMethods(name.toString())) try {
			return new JavaCall(method).invoke(args);
		} catch (ClassCastException | IllegalArgumentException ex) {}
		final var msg = "no such method found available: %s";
		throw new ElvaRuntimeException(msg, this.form(args));
	}

	/**
	 * 指定された実引数と評価器に対し、返り値を求めます。
	 *
	 *
	 * @param args 実引数
	 * @param eval 評価器
	 *
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	@Override
	public final Object apply(ListBase args, ElvaEval eval) {
		final var seq = args.map(eval);
		if(type != null) return invoke(type, seq);
		return invoke(TypeNode.of(seq.head()), seq);
	}
}
