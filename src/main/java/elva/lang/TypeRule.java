/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.lang;

/**
 * LISP処理系で処理系の外の値を取り込む際の暗黙的な型変換を定めます。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/10
 */
public interface TypeRule {
	/**
	 * 指定された値がこの型変換の対象になるか確認します。
	 *
	 *
	 * @param value 処理系の外部の値
	 *
	 * @return 対象の場合は真
	 */
	public abstract boolean support(Object value);

	/**
	 * この型変換に従って処理系の外の値を式に変換します。
	 *
	 *
	 * @param value 処理系の外部の値
	 *
	 * @return 式
	 */
	public abstract NodeBase encode(Object value);
}
