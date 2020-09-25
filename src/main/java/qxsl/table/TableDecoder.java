/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import qxsl.model.Item;

/**
 * 永続化された交信記録を読み取るためのデコーダです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/08
 */
public abstract class TableDecoder implements AutoCloseable {
	/**
	 * ストリームの交信記録を読み取ります。
	 *
	 *
	 * @return 読み取った交信記録
	 *
	 * @throws IOException 構文上または読取り時の例外
	 */
	public List<Item> decode() throws IOException {
		this.head();
		final var list = new ArrayList<Item>();
		while(hasNext()) list.add(this.next());
		this.foot();
		return list;
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	public abstract void close() throws IOException;

	/**
	 * ストリームの交信記録の冒頭を読み取ります。
	 *
	 *
	 * @throws IOException 構文上または読取り時の例外
	 *
	 * @since 2020/09/04
	 */
	public abstract void head() throws IOException;

	/**
	 * ストリームの交信記録の末尾を読み取ります。
	 *
	 *
	 * @throws IOException 構文上または読取り時の例外
	 *
	 * @since 2020/09/04
	 */
	public abstract void foot() throws IOException;

	/**
	 * ストリームの現在位置の交信記録を読み取ります。
	 *
	 *
	 * @return 読み取った交信記録
	 *
	 * @throws IOException 構文上または読取り時の例外
	 *
	 * @since 2020/09/04
	 */
	public abstract Item next() throws IOException;

	/**
	 * ストリームに交信記録が存在するかを確認します。
	 *
	 *
	 * @return 交信記録を読み取れる場合は真
	 *
	 * @throws IOException 構文上または読取り時の例外
	 *
	 * @since 2020/09/04
	 */
	public abstract boolean hasNext() throws IOException;
}
