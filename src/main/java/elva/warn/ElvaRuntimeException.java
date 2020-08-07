/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.warn;

import elva.lang.NodeBase;

import java.util.StringJoiner;

/**
 * LISP処理系で発生する実行時例外を表現します。
 * この例外はLISP処理系内部でのみ使用されます。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/01
 */
public final class ElvaRuntimeException extends RuntimeException {
	private static final String TEMPLATE = "runtime error: ";
	private final StringJoiner trace;
	private final String error;

	/**
	 * 問題を示す書式文字列とその引数を指定して例外を構築します。
	 *
	 *
	 * @param text 例外の内容
	 * @param args 書式文字列の引数
	 */
	public ElvaRuntimeException(String text, Object...args) {
		this.error = String.format(text, args);
		this.trace = new StringJoiner("\n");
		this.trace.add(TEMPLATE.concat(error));
	}

	/**
	 * 指定された例外を包む例外を構築します。
	 *
	 * @param ex 例外
	 */
	public ElvaRuntimeException(Throwable ex) {
		this(ex.toString());
		setStackTrace(ex.getStackTrace());
	}

	/**
	 * 指定された式をこの例外まで辿れる式の追跡履歴に追加します。
	 *
	 *
	 * @param sexp 追加する式
	 *
	 * @return この例外
	 */
	public final ElvaRuntimeException add(NodeBase sexp) {
		trace.add(" at ".concat(String.valueOf(sexp)));
		return this;
	}

	/**
	 * この例外の内容を示す最小限の文字列を返します。
	 *
	 * @return 例外の内容を示す文字列
	 */
	public final String getError() {
		return error;
	}

	/**
	 * この例外を処理系の外部に公開するための文字列を生成します。
	 *
	 * @return 例外の内容を示す文字列
	 */
	@Override
	public final String getMessage() {
		return trace.toString();
	}
}
