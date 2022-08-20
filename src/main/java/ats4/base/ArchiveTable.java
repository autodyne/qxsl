/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.base;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.List;

import qxsl.model.Item;
import qxsl.ruler.Pattern;
import qxsl.sheet.SheetOrTable;

import ats4.data.ArchiveData;
import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の交信履歴を管理する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class ArchiveTable extends AccountTable<ArchiveData> {
	private final Pattern rule;

	/**
	 * 指定されたデータベースを利用します。
	 *
	 *
	 * @param conn データベースの接続
	 * @param rule 交信記録の変換規則
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public ArchiveTable(Connection conn, Pattern rule) {
		super(conn, "ARCHIVE_DATA");
		this.rule = rule;
	}

	/**
	 * 検索結果に対応するレコードを構築します。
	 *
	 *
	 * @param rs 検索結果
	 *
	 * @return レコード
	 *
	 * @throws TableSchemaException 構造の問題
	 */
	@Override
	public final ArchiveData parse(ResultSet rs) {
		return new ArchiveData(rs);
	}

	/**
	 * 指定された呼出符号のレコードを返します。
	 *
	 *
	 * @param call 呼出符号
	 *
	 * @return 対応するレコード
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	public final List<ArchiveData> byCall(String call) {
		return new Select("call").value(call).execute();
	}

	/**
	 * 指定されたレコードが含む交信記録を解釈します。
	 *
	 *
	 * @param data レコード
	 * @return 交信記録
	 *
	 * @throws UncheckedIOException 未対応の書式の例外
	 *
	 * @since 2022/08/21
	 */
	public final List<Item> getItems(ArchiveData data) {
		try {
			final var sot = new SheetOrTable();
			final var seq = sot.unpack(data.data);
			return this.rule.normalize(seq, null);
		} catch (UncheckedIOException ex) {
			throw ex;
		} catch (RuntimeException ex) {
			final var io = new IOException(ex);
			throw new UncheckedIOException(io);
		}
	}
}
