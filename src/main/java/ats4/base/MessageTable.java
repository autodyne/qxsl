/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package ats4.base;

import java.io.UncheckedIOException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.LinkedList;
import java.util.List;

import qxsl.draft.Sign;
import qxsl.model.Item;
import qxsl.ruler.Pattern;
import qxsl.sheet.SheetOrTable;

import ats4.data.ArchiveData;
import ats4.data.MessageData;
import ats4.warn.TableAccessException;
import ats4.warn.TableSchemaException;

/**
 * 参加局の交信相手を管理する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/11
 */
public final class MessageTable extends AccountTable<MessageData> {
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
	public MessageTable(Connection conn, Pattern rule) {
		super(conn, "MESSAGE_DATA");
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
	public final MessageData parse(ResultSet rs) {
		return new MessageData(rs);
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
	public final List<MessageData> byCall(String call) {
		return new Select("call").value(call).execute();
	}

	/**
	 * 指定された交信記録のレコードを登録します。
	 *
	 *
	 * @param data レコード
	 *
	 * @throws UncheckedIOException 読み込みの例外
	 *
	 * @since 2022/08/21
	 */
	public final void push(ArchiveData data) {
		push(data.call, new SheetOrTable().unpack(data.data));
	}

	/**
	 * 相手局の記録との照合結果を設定して交信記録を返します。
	 *
	 *
	 * @param call 呼出符号
	 *
	 * @return 交信記録
	 *
	 * @see Sign
	 */
	public final List<Item> search(String call) {
		final var list = new LinkedList<Item>();
		for(final var data: this.byCall(call)) {
			final var v = verify(data);
			if(v != null) data.sign(v);
			list.add(data.item);
		}
		return list;
	}

	/**
	 * 指定されたレコードとの交信を表す交信記録を照合します。
	 *
	 *
	 * @param data レコード
	 *
	 * @return 対となるレコード
	 */
	public final MessageData verify(MessageData data) {
		final var query = new Select("dest");
		query.value(data.call.value());
		query.value(data.dest.value());
		for(final var v: query.execute()) {
			if(data.matches(v, rule)) return v;
		}
		return null;
	}

	/**
	 * 指定された交信記録のレコードを登録します。
	 *
	 *
	 * @param call 呼出符号
	 * @param list 交信記録
	 */
	public final void push(String call, List<Item> list) {
		for(var v: list) push(new MessageData(call, rule.normalize(v, null)));
	}

	/**
	 * 指定された交信記録のレコードを削除します。
	 *
	 *
	 * @param call 呼出符号
	 * @param list 交信記録
	 */
	public final void drop(String call, List<Item> list) {
		for(var v: list) drop(new MessageData(call, rule.normalize(v, null)));
	}
}
