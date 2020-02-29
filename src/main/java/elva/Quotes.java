/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

/**
 * LISP処理系で構文解析時に参照される特殊な引用演算子を列挙します。
 *
 *
 * @author Journal of Hamradio Informatics
 *
 * @since 2019/07/01
 */
enum Quotes {
	QUOTE ("quote"),
	UQUOT ("unquote"),
	QUASI ("quasiquote"),
	UQSPL ("unquote-splicing");

	private final Symbol name;
	private Quotes(String name) {
		this.name = new Symbol(name);
	}

	@Override
	public final String toString() {
		return name.toString();
	}

	public final Symbol toSymbol() {
		return name;
	}
}
