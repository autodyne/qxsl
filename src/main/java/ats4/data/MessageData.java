/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package ats4.data;

import java.io.UncheckedIOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import qxsl.draft.Call;
import qxsl.draft.Qxsl;
import qxsl.draft.Sign;
import qxsl.model.Item;
import qxsl.ruler.Pattern;

import gaas.table.ItemFactory;

import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の交信相手を格納するレコードです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/11
 */
public final class MessageData implements AccountData {
	/**
	 * 参加局の呼出符号です。
	 */
	public final Call call;

	/**
	 * 相手局の呼出符号です。
	 */
	public final Call dest;

	/**
	 * 参加局の交信記録です。
	 */
	public final Item item;

	/**
	 * レコードを構築します。
	 *
	 *
	 * @param call 呼出符号
	 * @param item 交信記録
	 */
	public MessageData(String call, Item item) {
		this.call = new Call(call);
		this.dest = (Call) item.get(Qxsl.CALL);
		this.item = item;
	}

	/**
	 * 指定された結果からレコードを構築します。
	 *
	 *
	 * @param rs 検索結果
	 *
	 * @throws TableSchemaException 構造の問題がある場合
	 *
	 * @throws UncheckedIOException 交信記録の変換の例外
	 */
	public MessageData(ResultSet rs) {
		try {
			this.call = new Call(rs.getString("call"));
			this.dest = new Call(rs.getString("dest"));
			this.item = decode(rs.getBytes("item"));
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
	 *
	 * @throws UncheckedIOException 交信記録の変換の例外
	 */
	@Override
	public final void copyTo(PreparedStatement ps) {
		try {
			ps.setString(1, call.value());
			ps.setString(2, dest.value());
			ps.setBytes(3, encode(item));
		} catch (SQLException ex) {
			throw new TableAccessException(ex);
		}
	}

	/**
	 * 指定されたバイト列を交信記録に変換します。
	 *
	 *
	 * @param data バイト表現
	 *
	 * @return 交信記録
	 *
	 * @throws UncheckedIOException 交信記録の変換の例外
	 */
	private final Item decode(byte[] data) {
		return new ItemFactory().decode(data).get(0);
	}

	/**
	 * 指定された交信記録をバイト列に変換します。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @return バイト表現
	 *
	 * @throws UncheckedIOException 交信記録の変換の例外
	 */
	private final byte[] encode(Item item) {
		return new ItemFactory().encode(item);
	}

	/**
	 * 指定されたレコードとの照合の成立を交信記録に設定します。
	 *
	 *
	 * @param data 対となるレコード
	 *
	 * @see Sign
	 */
	public final void sign(MessageData data) {
		this.item.set(new Sign(data.item));
	}

	/**
	 * 指定されたレコードがこのレコードと対をなすか検証します。
	 *
	 *
	 * @param data 照合するレコード
	 * @param rule 照合の基準
	 *
	 * @return 対をなす場合は真
	 */
	public final boolean matches(MessageData data, Pattern rule) {
		if(!this.call.equals(data.dest)) return false;
		if(!data.call.equals(this.dest)) return false;
		return rule.match(this.item, data.item);
	}
}
