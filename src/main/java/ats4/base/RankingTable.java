/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package ats4.base;

import java.sql.Connection;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import qxsl.ruler.Contest;
import qxsl.ruler.Section;

import ats4.data.RankingData;
import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の成績順位を管理する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class RankingTable extends AccountTable<RankingData> {
	/**
	 * 指定されたデータベースを利用します。
	 *
	 *
	 * @param conn データベースの接続
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public RankingTable(Connection conn) {
		super(conn, "RANKING_DATA");
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
	public final RankingData parse(ResultSet rs) {
		return new RankingData(rs);
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
	public final List<RankingData> byCall(String call) {
		return new Select("call").value(call).execute();
	}

	/**
	 * 指定された登録部門のレコードを返します。
	 *
	 *
	 * @param sect 登録部門
	 *
	 * @return 対応するレコード
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	public final List<RankingData> bySect(String sect) {
		return new Select("sect").value(sect).execute();
	}

	/**
	 * 指定された登録部門のレコードを返します。
	 *
	 *
	 * @param sect 登録部門
	 *
	 * @return 対応するレコード
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	public final List<RankingData> bySect(Section sect) {
		return bySect(sect.name());
	}

	/**
	 * どの部門にも不参加の呼出符号を返します。
	 *
	 *
	 * @param rule 規約
	 *
	 * @return 見つかった呼出符号
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 *
	 * @since 2025/07/30
	 */
	public final List<String> absence(Contest rule) {
		final var map = new HashMap<String, Boolean>();
		for(var v: this.list()) {
			boolean test = rule.section(v.sect).isAbsence();
			map.merge(v.call, test, Boolean::logicalAnd);
		}
		var set = map.keySet().stream().filter(map::get);
		return set.sorted().collect(Collectors.toList());
	}

	/**
	 * 指定された登録部門の入賞局数を返します。
	 *
	 *
	 * @param sect 登録部門
	 *
	 * @return 入賞する参加局の数
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	public final int getAwardLimit(Section sect) {
		final var group = bySect(sect.name()).stream();
		final var total = group.mapToInt(e -> e.total);
		return sect.getAwardLimit(total.toArray());
	}
}
