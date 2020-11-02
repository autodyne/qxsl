/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

/**
 * コンテストの部門はこのクラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Section extends Formula {
	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 名前
	 */
	@Override
	public final String toString() {
		return name();
	}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 名前
	 */
	public abstract String name();

	/**
	 * 部門の番号を返します。
	 *
	 *
	 * @return 番号
	 */
	public abstract String code();
}
