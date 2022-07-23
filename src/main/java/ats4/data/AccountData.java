/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.data;

import java.io.Serializable;
import java.sql.PreparedStatement;

import ats4.warn.TableAccessException;

/**
 * 参加局の登録情報を格納する共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public interface AccountData extends Serializable {
	/**
	 * このレコードを登録する命令を設定します。
	 *
	 *
	 * @param ps クエリ
	 *
	 * @throws TableAccessException 疎通の障害がある場合
	 */
	public abstract void copyTo(PreparedStatement ps);
}
