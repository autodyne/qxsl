/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package ats4.data;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.function.Predicate;

import qxsl.ruler.Contest;
import qxsl.ruler.Summary;

import ats4.base.RankingTable;
import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の成績順位を格納するレコードです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public final class RankingData implements AccountData {
	/**
	 * 参加局の呼出符号です。
	 */
	public String call;

	/**
	 * 参加局の登録部門です。
	 */
	public String sect;

	/**
	 * 参加局の運用場所です。
	 */
	public String city;

	/**
	 * 参加局の素点です。
	 */
	public int score;

	/**
	 * 参加局の得点です。
	 */
	public int total;

	/**
	 * レコードを構築します。
	 */
	public RankingData() {}

	/**
	 * 指定された結果からレコードを構築します。
	 *
	 *
	 * @param rs 検索結果
	 *
	 * @throws TableSchemaException 構造の問題がある場合
	 */
	public RankingData(ResultSet rs) {
		try {
			this.call = rs.getString("call");
			this.sect = rs.getString("sect");
			this.city = rs.getString("city");
			this.score = rs.getInt("score");
			this.total = rs.getInt("total");
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
			ps.setString(2, this.sect);
			ps.setString(3, this.city);
			ps.setInt(4, this.score);
			ps.setInt(5, this.total);
		} catch (SQLException ex) {
			throw new TableAccessException(ex);
		}
	}

	/**
	 * 指定された採点結果から得点を取得します。
	 *
	 *
	 * @param summary 採点結果
	 *
	 * @return このレコード
	 */
	public final RankingData copy(Summary summary) {
		this.score = summary.score();
		this.total = summary.total();
		return this;
	}

	/**
	 * 総合得点の優劣を比較する関数を返します。
	 *
	 *
	 * @return このレコードが劣る場合は真を返す関数
	 */
	private final Predicate<RankingData> defeated() {
		return data -> data.total > this.total;
	}

	/**
	 * 指定されたテーブルで総合順位を返します。
	 *
	 *
	 * @param table 検索対象のテーブル
	 *
	 * @return 順位
	 *
	 * @throws TableAccessException 疎通の障害
	 * @throws TableSchemaException 構造の問題
	 */
	public final int getRankFromZeroIn(RankingTable table) {
		final var entries = table.bySect(this.sect).stream();
		return (int) entries.filter(this.defeated()).count();
	}

	/**
	 * このレコードが無得点を示すか確認します。
	 *
	 *
	 * @param contest 規約
	 *
	 * @return 該当の参加部門が存在し、無得点の場合は真
	 *
	 * @since 2023/01/08
	 */
	public final boolean scoreless(Contest contest) {
		try {
			final var rule = contest.section(sect);
			return !rule.isAbsence() && score == 0;
		} catch (NullPointerException ex) {
			return false;
		}
	}
}
