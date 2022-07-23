/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.base;

import java.sql.Connection;
import java.sql.ResultSet;
import java.util.List;
import java.util.UUID;

import ats4.data.StationData;
import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の連絡先等を管理する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class StationTable extends AccountTable<StationData> {
	/**
	 * 指定されたデータベースを利用します。
	 *
	 *
	 * @param conn データベースの接続
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public StationTable(Connection conn) {
		super(conn, "STATION_DATA");
	}

	/**
	 * 重複が取り除かれた識別番号を発行します。
	 *
	 *
	 * @return 識別番号
	 */
	public final UUID createUUID() {
		final UUID v = UUID.randomUUID();
		if(byUUID(v).isEmpty()) return v;
		return createUUID();
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
	public final StationData parse(ResultSet rs) {
		return new StationData(rs);
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
	public final List<StationData> byCall(String call) {
		return new Select("call").value(call).execute();
	}

	/**
	 * 指定された識別番号のレコードを返します。
	 *
	 *
	 * @param uuid 識別番号
	 *
	 * @return 対応するレコード
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	public final List<StationData> byUUID(UUID uuid) {
		return byUUID(uuid.toString());
	}

	/**
	 * 指定された識別番号のレコードを返します。
	 *
	 *
	 * @param uuid 識別番号
	 *
	 * @return 対応するレコード
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	public final List<StationData> byUUID(String uuid) {
		return new Select("uuid").value(uuid).execute();
	}
}
