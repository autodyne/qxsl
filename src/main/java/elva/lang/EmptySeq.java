/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.Iterator;

/**
 * LISP処理系内部で利用される空のリスト構造の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class EmptySeq extends ListBase {
	/**
	 * 内容が空のリストを示す特別なインスタンスです。
	 */
	public static final EmptySeq NIL = new EmptySeq();

	/**
	 * このコンストラクタは隠蔽されます。
	 */
	private EmptySeq() {}

	/**
	 * 空のリストを返します。
	 *
	 *
	 * @return 先頭
	 */
	@Override
	public final NodeBase head() {
		return this;
	}

	/**
	 * 空のリストを返します。
	 *
	 *
	 * @return 後続
	 */
	@Override
	public final ListBase tail() {
		return this;
	}

	/**
	 * 空の部分リストを返します。
	 *
	 *
	 * @param skip 部分リストが始まる位置
	 *
	 * @return 部分リスト
	 */
	@Override
	public final ListBase drop(int skip) {
		return this;
	}

	/**
	 * 空の部分リストを返します。
	 *
	 *
	 * @param size 部分リストが終わる位置
	 *
	 * @return 部分リスト
	 */
	@Override
	public final ListBase take(int size) {
		return this;
	}

	/**
	 * 例外を発生します。
	 *
	 *
	 * @param index 要素の位置
	 *
	 * @return 要素
	 */
	@Override
	public final NodeBase get(int index) {
		final String msg = String.valueOf(index);
		throw new IndexOutOfBoundsException(msg);
	}

	/**
	 * このリストの要素数を返します。
	 *
	 *
	 * @return 要素数
	 */
	@Override
	public final int size() {
		return 0;
	}

	/**
	 * リストの内容を列挙するイテレータを返します。
	 *
	 *
	 * @return イテレータ
	 */
	@Override
	public final Iterator<NodeBase> iterator() {
		return new ChainIt(this);
	}
}
