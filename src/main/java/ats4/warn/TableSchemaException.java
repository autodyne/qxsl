/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.warn;

import java.sql.SQLException;

/**
 * データベースの構造の問題を報告する例外です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class TableSchemaException extends RuntimeException {
	/**
	 * 指定された例外を包みます。
	 *
	 *
	 * @param ex 原因となった例外
	 */
	public TableSchemaException(SQLException ex) {
		super(ex);
	}
}
