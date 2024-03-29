/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.lang;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系で利用される演算子の共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
public abstract class FormBase extends AtomBase {
	/**
	 * 演算子が取る引数の最大の個数です。
	 */
	public static final int MAX = 256;

	/**
	 * この演算子が可変長引数の演算子か確認します。
	 *
	 *
	 * @return 可変長引数なら真
	 */
	public abstract boolean isVarArgs();

	/**
	 * この演算子が取る引数の最小の個数を返します。
	 *
	 *
	 * @return 最小限の引数の個数
	 */
	public abstract int getMinimumArgumentLength();

	/**
	 * この演算子が取る引数の最大の個数を返します。
	 *
	 *
	 * @return 最大限の引数の個数
	 */
	public abstract int getMaximumArgumentLength();

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
	public abstract Object apply(ListBase args, ElvaEval eval);
}
