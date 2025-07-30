/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package ats4.root;

import java.util.StringJoiner;

import qxsl.ruler.Contest;
import qxsl.ruler.Program;
import qxsl.ruler.Section;

import ats4.base.RankingTable;
import ats4.base.StationTable;
import ats4.data.RankingData;
import ats4.data.StationData;
import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の情報をCSVに出力する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2024/07/14
 */
public final class CSV {
	private final Program sections;
	private final RankingTable rankings;
	private final StationTable stations;

	/**
	 * 指定されたデータベースと規約を使用します。
	 *
	 *
	 * @param ats データベース
	 * @param program 規約
	 */
	public CSV(ATS ats, Program program) {
		this.sections = program;
		this.rankings = ats.rankings();
		this.stations = ats.stations();
	}

	/**
	 * 現時点での集計結果を出力します。
	 *
	 *
	 * @return CSVの文字列
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	public final String dump() {
		final var join = new StringJoiner("\n");
		contest(sections, join);
		return join.toString();
	}

	/**
	 * 指定された規約の結果を出力します。
	 *
	 *
	 * @param rule 規約
	 * @param join 出力
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	private final void contest(Contest rule, StringJoiner join) {
		for(var sec: rule) if(!sec.isAbsence()) section(sec, join);
		for(var row: rankings.absence(rule)) join.add(escape(row));
	}

	/**
	 * 指定された部門の結果を出力します。
	 *
	 *
	 * @param rule 部門
	 * @param join 出力
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	private final void section(Section rule, StringJoiner join) {
		for(var row: rankings.bySect(rule)) join.add(ranking(row));
	}

	/**
	 * 指定された順位の結果を出力します。
	 *
	 *
	 * @param ranking 順位
	 *
	 * @return CSVの文字列
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	private final String ranking(RankingData ranking) {
		final var station = resolve(ranking.call);
		final var columns = new StringJoiner(",");
		columns.add(escape(station.call));
		columns.add(escape(ranking.city));
		columns.add(escape(ranking.sect));
		columns.add(Integer.toString(rank(ranking)));
		columns.add(Boolean.toString(pass(ranking)));
		columns.add(Integer.toString(ranking.total));
		columns.add(escape(station.name));
		columns.add(escape(station.post));
		columns.add(escape(station.addr));
		columns.add(escape(station.mail));
		columns.add(escape(station.note));
		return columns.toString();
	}

	/**
	 * 指定された文字列をエスケープ処理します。
	 *
	 *
	 * @param value 文字列
	 *
	 * @return エスケープされた文字列
	 */
	private final String escape(String value) {
		value = value.replace("\"", "\"\"");
		value = value.replace("\n", "");
		value = value.replace("\r", "");
		return "\"".concat(value).concat("\"");
	}

	/**
	 * 指定された呼出符号の参加局を検索します。
	 *
	 *
	 * @param call 呼出符号
	 *
	 * @return 参加局
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	private final StationData resolve(String call) {
		return stations.byCall(call).get(0);
	}

	/**
	 * 指定された参加局の総合順位を計算します。
	 *
	 *
	 * @param ranking 参加局
	 *
	 * @return 順位
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	private final int rank(RankingData ranking) {
		return ranking.getRankFromZeroIn(rankings) + 1;
	}

	/**
	 * 指定された参加局が入賞するか確認します。
	 *
	 *
	 * @param ranking 参加局
	 *
	 * @return 入賞する場合は真
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	private final boolean pass(RankingData ranking) {
		final var section = this.sections.section(ranking.sect);
		return rank(ranking) <= rankings.getAwardLimit(section);
	}
}
