/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package ats4.base;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

import qxsl.utils.AssetUtil;

import ats4.data.AccountData;
import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の登録情報を管理する共通実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/18
 *
 * @param <R> レコードの総称型
 */
public abstract class AccountTable<R extends AccountData> {
	private final Map<String, PreparedStatement> cmds;
	private final Properties raws;
	private final Connection conn;

	/**
	 * 指定されたデータベースを利用します。
	 *
	 *
	 * @param conn データベースの接続
	 * @param name テーブルの名前
	 */
	public AccountTable(Connection conn, String name) {
		this.raws = AssetUtil.from(this).properties(name);
		this.cmds = new ConcurrentHashMap<>();
		this.conn = conn;
	}

	/**
	 * このテーブルを新規に作成します。
	 *
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final void createTable() {
		new Update("createTable").execute();
	}

	/**
	 * このテーブルを完全に消去します。
	 *
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final void deleteTable() {
		new Update("deleteTable").execute();
	}

	/**
	 * 指定されたレコードを追加します。
	 *
	 *
	 * @param data 追加するレコード
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final void push(R data) {
		new Update("push").value(data).execute();
	}

	/**
	 * 指定されたレコードを削除します。
	 *
	 *
	 * @param data 削除するレコード
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final void drop(R data) {
		new Update("drop").value(data).execute();
	}

	/**
	 * 登録されたレコードを列挙します。
	 *
	 *
	 * @return レコードのリスト
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	public final List<R> list() {
		return new Select("list").execute();
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
	public abstract R parse(ResultSet rs);

	/**
	 * 指定された名前の更新クエリを実行します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2022/07/18
	 */
	public final class Update {
		private final PreparedStatement ps;

		/**
		 * 指定されたクエリを設定します。
		 *
		 *
		 * @param name クエリの名前
		 */
		public Update(String name) {
			this.ps = prepare(name);
		}

		/**
		 * 指定されたレコードを写します。
		 *
		 *
		 * @param value 登録するレコード
		 *
		 * @return このオブジェクト
		 *
		 * @throws TableAccessException 疎通の障害
		 */
		public final Update value(R value) {
			value.copyTo(this.ps);
			return this;
		}

		/**
		 * 設定されたクエリを実行します。
		 *
		 *
		 * @throws TableAccessException 疎通の障害
		 */
		public final void execute() {
			try {
				ps.executeUpdate();
			} catch (SQLException ex) {
				throw new TableAccessException(ex);
			}
		}
	}

	/**
	 * 指定された名前の検索クエリを実行します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2022/07/17
	 */
	public final class Select {
		private final PreparedStatement ps;
		private int cnt;

		/**
		 * 指定されたクエリを設定します。
		 *
		 *
		 * @param name クエリの名前
		 */
		public Select(String name) {
			this.ps = prepare(name);
		}

		/**
		 * 指定された属性値を設定します。
		 *
		 *
		 * @param value 検索する値
		 *
		 * @return このオブジェクト
		 *
		 * @throws TableAccessException 疎通の障害
		 */
		public final Select value(String value) {
			try {
				this.ps.setString(++cnt, value);
			} catch (SQLException ex) {
				throw new TableAccessException(ex);
			}
			return this;
		}

		/**
		 * 設定されたクエリを実行します。
		 *
		 *
		 * @return 検索結果のレコード
		 *
		 * @throws TableAccessException 疎通の障害
		 */
		public final List<R> execute() {
			try(final var rs = ps.executeQuery()) {
				final var list = new LinkedList<R>();
				while(rs.next()) list.add(parse(rs));
				return list;
			} catch (SQLException ex) {
				throw new TableAccessException(ex);
			}
		}
	}

	/**
	 * 指定されたクエリをコンパイルします。
	 *
	 *
	 * @param name クエリの名前
	 *
	 * @return 命令
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	private final PreparedStatement compile(String name) {
		try {
			final var sql = raws.getProperty(name);
			return this.conn.prepareStatement(sql);
		} catch (SQLException ex) {
			throw new TableAccessException(ex);
		}
	}

	/**
	 * 指定されたクエリをコンパイルします。
	 * また、その結果はキャッシュされます。
	 *
	 *
	 * @param name クエリの名前
	 *
	 * @return 命令
	 *
	 * @throws TableAccessException 疎通の障害
	 */
	private final PreparedStatement prepare(String name) {
		return cmds.computeIfAbsent(name, this::compile);
	}
}
