/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.data;

import java.io.UncheckedIOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import qxsl.model.Item;
import qxsl.sheet.SheetOrTable;

import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の交信履歴を格納するレコードです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class ArchiveData implements AccountData {
	/**
	 * 交信履歴の呼出符号です。
	 */
	public String call;

	/**
	 * 交信履歴のファイルの名前です。
	 */
	public String file;

	/**
	 * 交信履歴のファイルの内容です。
	 */
	public byte[] data;

	/**
	 * レコードを構築します。
	 */
	public ArchiveData() {}

	/**
	 * 指定された結果からレコードを構築します。
	 *
	 *
	 * @param rs 検索結果
	 *
	 * @throws TableSchemaException 構造の問題がある場合
	 */
	public ArchiveData(ResultSet rs) {
		try {
			this.call = rs.getString("call");
			this.file = rs.getString("file");
			this.data = rs.getBytes("data");
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
			ps.setString(2, this.file);
			ps.setBytes(3, this.data);
		} catch (SQLException ex) {
			throw new TableAccessException(ex);
		}
	}

	/**
	 * このレコードが含む交信記録を解釈します。
	 *
	 *
	 * @return 交信記録
	 *
	 * @throws UncheckedIOException 未対応の書式の例外
	 */
	public final List<Item> toItemList() {
		final var cl = getClass().getClassLoader();
		return new SheetOrTable(cl).unpack(data);
	}

	/**
	 * 交信記録のエラーを表す文字列を返します。
	 *
	 *
	 * @return 警告の文字列
	 */
	public final String getWarningMessage() {
		try {
			toItemList();
			return null;
		} catch (UncheckedIOException ex) {
			return ex.getCause().getMessage();
		}
	}
}