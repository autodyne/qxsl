/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

/**
 * 得点計算の処理に失敗した場合に返されます。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2016/11/26
 */
public final class Failure implements Message {
	private static final long serialVersionUID = 1L;
	private final Exception cause;
	private final String message;
	
	/**
	 * 指定した内容の{@link Failure}を構築します。
	 *
	 * @param message 失敗の内容を説明する文字列
	 */
	public Failure(String message) {
		this.cause = null;
		this.message = message;
	}

	/**
	 * 指定した例外で{@link Failure}を構築します。
	 *
	 * @param cause 失敗の原因となった例外
	 */
	public Failure(Exception cause) {
		this.cause = cause;
		this.message = cause.getMessage();
	}

	/**
	 * 失敗の内容を説明する文字列を返します。
	 * 
	 * @return メッセージ
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * 失敗の原因となった例外を返します。
	 *
	 * @return 例外
	 */
	public Exception getCause() {
		return cause;
	}
}
