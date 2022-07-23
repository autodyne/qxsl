/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.data;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の連絡先等を格納するレコードです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class StationData implements AccountData {
	/**
	 * 参加局の呼出符号です。
	 */
	public String call;

	/**
	 * 参加局の宛先氏名です。
	 */
	public String name;

	/**
	 * 参加局の郵便番号です。
	 */
	public String post;

	/**
	 * 参加局の宛先住所です。
	 */
	public String addr;

	/**
	 * 参加局のメール先です。
	 */
	public String mail;

	/**
	 * 参加局のトークンです。
	 */
	public String uuid;

	/**
	 * 参加局のコメントです。
	 */
	public String note;

	/**
	 * レコードを構築します。
	 */
	public StationData() {}

	/**
	 * 指定された結果からレコードを構築します。
	 *
	 *
	 * @param rs 検索結果
	 *
	 * @throws TableSchemaException 構造の問題がある場合
	 */
	public StationData(ResultSet rs) {
		try {
			this.call = rs.getString("call");
			this.name = rs.getString("name");
			this.post = rs.getString("post");
			this.addr = rs.getString("addr");
			this.mail = rs.getString("mail");
			this.uuid = rs.getString("uuid");
			this.note = rs.getString("note");
		} catch (SQLException ex) {
			throw new TableSchemaException(ex);
		}
	}

	/**
	 * このレコードを登録する命令を設定します。
	 *
	 *
	 * @param ps クエリ
	 *
	 * @throws TableAccessException 疎通の障害がある場合
	 */
	@Override
	public final void copyTo(PreparedStatement ps) {
		try {
			ps.setString(1, this.call);
			ps.setString(2, this.name);
			ps.setString(3, this.post);
			ps.setString(4, this.addr);
			ps.setString(5, this.mail);
			ps.setString(6, this.uuid);
			ps.setString(7, this.note);
		} catch (SQLException ex) {
			throw new TableAccessException(ex);
		}
	}
}
