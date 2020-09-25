/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.Serializable;

import qxsl.model.Item;

/**
 * 規約により受理もしくは拒否された交信に付与されます。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/12/05
 */
public interface Message extends Serializable {
	/**
	 * 交信の成立により得られる素点を返します。
	 *
	 *
	 * @return 交信1件の得点
	 */
	public int score();

	/**
	 * 交信の実体を表す{@link Item}を返します。
	 *
	 *
	 * @return 交信の実体
	 */
	public Item item();

	/**
	 * 処理の結果を説明する文字列を返します。
	 *
	 *
	 * @return 文字列
	 */
	public String text();

	/**
	 * 総得点や乗数の計算に使用される識別子を返します。
	 *
	 *
	 * @param keyNum 識別子の配列内の位置
	 *
	 * @return 指定された位置にある識別子
	 *
	 * @throws IndexOutOfBoundsException 範囲外の場合
	 */
	public Object key(int keyNum);

	/**
	 * この交信に関連づけられた識別子の個数を返します。
	 *
	 *
	 * @return 乗数の個数
	 */
	public int size();
}
