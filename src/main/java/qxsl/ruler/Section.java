/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

/**
 * コンテストの部門の実装はこの抽象クラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Section extends Counter {
	/**
	 * 部門を構築します。
	 */
	public Section() {}

	/**
	 * 部門の番号を返します。
	 *
	 *
	 * @return 部門の番号
	 */
	public abstract String getCode();
}
