/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系からフィールドを参照する演算子の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
public final class FlexLoad extends FormBase {
	private final String name;

	/**
	 * 指定された名前のフィールドを参照します。
	 *
	 *
	 * @param name フィールドの名前
	 */
	public FlexLoad(NameNode name) {
		this.name = name.toString();
	}

	/**
	 * この演算子の実体を返します。
	 *
	 *
	 * @return 演算子の実体
	 */
	@Override
	public final FlexLoad value() {
		return this;
	}

	/**
	 * この演算子が可変長引数のフィールドか確認します。
	 *
	 *
	 * @return 常に偽
	 */
	@Override
	public final boolean isVarArgs() {
		return false;
	}

	/**
	 * このフィールドが取る引数の最小の個数を返します。
	 *
	 *
	 * @return 常に1
	 */
	@Override
	public final int getMinimumArgumentLength() {
		return 1;
	}

	/**
	 * このフィールドが取る引数の最大の個数を返します。
	 *
	 *
	 * @return 常に1
	 */
	@Override
	public final int getMaximumArgumentLength() {
		return 1;
	}

	/**
	 * このフィールドを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return String.format("(access '%s)", name);
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
		final var obj = eval.apply(args.head());
		final var fld = TypeNode.of(obj).getField(name);
		return new JavaLoad(fld).invoke(obj);
	}
}
