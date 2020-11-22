/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系からコンストラクタを参照する演算子の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
public final class FlexInit extends FormBase {
	private final TypeNode type;

	/**
	 * 指定された型のコンストラクタを参照します。
	 *
	 *
	 * @param type コンストラクタの型
	 */
	public FlexInit(TypeNode type) {
		this.type = type;
	}

	/**
	 * この演算子の実体を返します。
	 *
	 *
	 * @return 演算子の実体
	 */
	@Override
	public final FlexInit value() {
		return this;
	}

	/**
	 * この演算子が可変長引数のコンストラクタか確認します。
	 *
	 *
	 * @return 常に偽
	 */
	@Override
	public final boolean isVarArgs() {
		return false;
	}

	/**
	 * このコンストラクタが取る引数の最小の個数を返します。
	 *
	 *
	 * @return 最小限の引数の個数
	 */
	@Override
	public final int getMinimumArgumentLength() {
		return 0;
	}

	/**
	 * このコンストラクタが取る引数の最大の個数を返します。
	 *
	 *
	 * @return 最大限の引数の個数
	 */
	@Override
	public final int getMaximumArgumentLength() {
		return MAX;
	}

	/**
	 * このコンストラクタを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return String.format("(new %s)", type);
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
		for(var cons: type.getConstructors()) try {
			return new JavaInit(cons).apply(args, eval);
		} catch (ClassCastException ex) {}
		final var msg = "no constructor found available: %s";
		throw new ElvaRuntimeException(msg, this.form(args));
	}
}
