/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
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
