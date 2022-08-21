/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.data;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import qxsl.model.Item;
import qxsl.table.TableManager;

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
	 * 指定された交信記録を設定します。
	 *
	 *
	 * @param list 交信記録
	 *
	 * @return このレコード
	 *
	 * @since 2022/08/21
	 */
	public final ArchiveData load(List<Item> list) {
		this.data = new TableManager().encode(list);
		return this;
	}

	/**
	 * 指定されたパスの交信記録を設定します。
	 *
	 *
	 * @param path 交信記録のパス
	 *
	 * @return このレコード
	 *
	 * @throws UncheckedIOException 読み取りの例外
	 *
	 * @since 2022/08/21
	 */
	public final ArchiveData load(Path path) {
		try {
			this.data = Files.readAllBytes(path);
			this.file = String.valueOf(path);
			return this;
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}
}
