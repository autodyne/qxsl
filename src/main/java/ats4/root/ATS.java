/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package ats4.root;

import java.sql.Connection;
import java.sql.SQLException;

import qxsl.ruler.Contest;
import qxsl.ruler.Pattern;
import qxsl.ruler.RuleKit;

import ats4.base.ArchiveTable;
import ats4.base.MessageTable;
import ats4.base.RankingTable;
import ats4.base.StationTable;
import ats4.warn.TableAccessException;

/**
 * 参加局の情報を管理する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class ATS implements AutoCloseable {
	private final Pattern rule;
	private final Connection conn;
	private final ArchiveTable archives;
	private final MessageTable messages;
	private final RankingTable rankings;
	private final StationTable stations;

	/**
	 * 指定されたデータベースを利用します。
	 *
	 *
	 * @param conn データベースの接続
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public ATS(Connection conn) {
		this(conn, RuleKit.load("format.lisp").pattern());
	}

	/**
	 * 指定されたデータベースを利用します。
	 *
	 *
	 * @param conn データベースの接続
	 * @param rule 交信記録の変換規則
	 *
	 * @throws TableAccessException 疎通の障害
	 *
	 * @since 2022/08/11
	 */
	public ATS(Connection conn, Pattern rule) {
		try {
			this.archives = new ArchiveTable(conn, rule);
			this.messages = new MessageTable(conn, rule);
			this.rankings = new RankingTable(conn);
			this.stations = new StationTable(conn);
		} finally {
			this.rule = rule;
			this.conn = conn;
		}
	}

	/**
	 * データベースの接続状態を解消します。
	 *
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final void close() {
		try {
			conn.close();
		} catch (SQLException ex) {
			throw new TableAccessException(ex);
		}
	}

	/**
	 * 交信履歴を管理するテーブルを返します。
	 *
	 *
	 * @return 交信履歴を管理するテーブル
	 */
	public final ArchiveTable archives() {
		return archives;
	}

	/**
	 * 交信相手を管理するテーブルを返します。
	 *
	 *
	 * @return 交信相手を管理するテーブル
	 */
	public final MessageTable messages() {
		return messages;
	}

	/**
	 * 成績順位を管理するテーブルを返します。
	 *
	 *
	 * @return 成績順位を管理するテーブル
	 */
	public final RankingTable rankings() {
		return rankings;
	}

	/**
	 * 連絡先等を管理するテーブルを返します。
	 *
	 *
	 * @return 連絡先等を管理するテーブル
	 */
	public final StationTable stations() {
		return stations;
	}

	/**
	 * 全てのテーブルを作成します。
	 *
	 *
	 * @return この接続
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final ATS createTables() {
		archives.createTable();
		messages.createTable();
		rankings.createTable();
		stations.createTable();
		return this;
	}

	/**
	 * 全てのテーブルを消去します。
	 *
	 *
	 * @return この接続
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final ATS deleteTables() {
		archives.deleteTable();
		messages.deleteTable();
		rankings.deleteTable();
		stations.deleteTable();
		return this;
	}

	/**
	 * 指定された呼出符号のレコードを全て削除します。
	 *
	 *
	 * @param call 削除する呼出符号
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final void drop(String call) {
		for(var e: archives.byCall(call)) archives.drop(e);
		for(var e: messages.byCall(call)) messages.drop(e);
		for(var e: rankings.byCall(call)) rankings.drop(e);
		for(var e: stations.byCall(call)) stations.drop(e);
	}

	/**
	 * 指定された呼出符号の参加局の得点を更新します。
	 *
	 *
	 * @param call 呼出符号
	 * @param rule コンテストの規約
	 *
	 * @throws TableAccessException 疎通の障害
	 *
	 * @since 2022/08/21
	 */
	public final void update(String call, Contest rule) {
		final var seq = messages.search(call);
		for(var ranking: rankings.byCall(call)) {
			final var sect = rule.section(ranking.sect);
			rankings.drop(ranking);
			ranking.copy(sect.summarize(seq));
			rankings.push(ranking);
		}
	}

	/**
	 * 全ての参加局の得点を更新します。
	 *
	 *
	 * @param rule コンテストの規約
	 *
	 * @throws TableAccessException 疎通の障害
	 *
	 * @since 2024/07/16
	 */
	public final void updateAll(Contest rule) {
		for(var message: messages.list()) messages.drop(message);
		for(var archive: archives.list()) messages.push(archive);
		for(var station: stations.list()) update(station.call, rule);
	}
}
