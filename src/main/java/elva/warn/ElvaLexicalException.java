/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.warn;

import elva.lang.ElvaLisp.Lexical;

/**
 * LISP処理系で発生する構文の例外を表現します。
 * この例外はLISP処理系内部でのみ使用されます。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/01
 */
public final class ElvaLexicalException extends RuntimeException {
	private static final String TEMPLATE = "lexical error: %s%n%s";

	/**
	 * 字句番号と内容を示す文字列を指定して例外を構築します。
	 *
	 *
	 * @param text 例外の内容
	 * @param scan 構文解析器
	 */
	public ElvaLexicalException(String text, Lexical scan) {
		super(String.format(TEMPLATE, text, scan.getLocal()));
	}
}
