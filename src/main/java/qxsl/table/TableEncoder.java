/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.util.List;

import qxsl.model.Item;

/**
 * 交信記録を所定の書式で永続化するためのエンコーダです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/08
 */
public abstract class TableEncoder implements AutoCloseable {
	private int count = -1;

	/**
	 * ストリームに交信記録を書き込みます。
	 *
	 *
	 * @param items 書き込む交信記録
	 *
	 * @throws IOException 書き込み時の例外
	 */
	public void encode(List<Item> items) throws IOException {
		this.count = items.size();
		this.head();
		for(var item: items) verify(item);
		for(var item: items) output(item);
		this.foot();
	}

	/**
	 * 交信記録の件数または負の整数値を返します。
	 *
	 *
	 * @return 交信記録の件数
	 */
	public final int count() {
		return count;
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	public abstract void close() throws IOException;

	/**
	 * ストリームに交信記録の冒頭を書き込みます。
	 *
	 *
	 * @throws IOException 書き込み時の例外
	 *
	 * @since 2020/09/04
	 */
	public abstract void head() throws IOException;

	/**
	 * ストリームに交信記録の末尾を書き込みます。
	 *
	 *
	 * @throws IOException 書き込み時の例外
	 *
	 * @since 2020/09/04
	 */
	public abstract void foot() throws IOException;

	/**
	 * ストリームに書き込まずに交信記録を検査します。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2020/09/04
	 */
	public abstract void verify(Item item) throws IOException;

	/**
	 * ストリームの現在位置に交信記録を書き込みます。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @throws IOException 書き込み時の例外
	 *
	 * @since 2020/09/04
	 */
	public abstract void output(Item item) throws IOException;
}
