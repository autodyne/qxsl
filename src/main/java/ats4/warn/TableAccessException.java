/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.warn;

import java.sql.SQLException;

/**
 * データベースの疎通の障害を報告する例外です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class TableAccessException extends RuntimeException {
	/**
	 * 指定された例外を包みます。
	 *
	 *
	 * @param ex 原因となった例外
	 */
	public TableAccessException(SQLException ex) {
		super(ex);
	}
}
