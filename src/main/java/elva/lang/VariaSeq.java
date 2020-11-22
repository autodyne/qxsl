/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.Iterator;
import java.util.function.UnaryOperator;

/**
 * LISP処理系内部で利用される可変長引数の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/08/02
 */
public final class VariaSeq extends ListBase {
	private final ListBase base;
	private final int size;

	/**
	 * 内容と長さを指定してリストを構築します。
	 *
	 *
	 * @param base 内容
	 * @param size 長さ
	 */
	public VariaSeq(ListBase base, int size) {
		this.base = base;
		this.size = size;
	}

	/**
	 * このリストの先頭を返します。
	 *
	 *
	 * @return 先頭
	 */
	@Override
	public final NodeBase head() {
		return isEmpty()? NIL: get(0);
	}

	/**
	 * このリストの後続を返します。
	 *
	 *
	 * @return 後続
	 */
	@Override
	public final ListBase tail() {
		return new VariaSeq(base.tail(), size - 1);
	}

	/**
	 * 指定された位置で始まる部分リストを返します。
	 *
	 *
	 * @param skip 部分リストが始まる位置
	 *
	 * @return 部分リスト
	 */
	@Override
	public final ListBase drop(int skip) {
		return new VariaSeq(base.drop(skip), size - skip);
	}

	/**
	 * 指定された位置で終わる部分リストを返します。
	 *
	 *
	 * @param size 部分リストが終わる位置
	 *
	 * @return 部分リスト
	 */
	@Override
	public final ListBase take(int size) {
		return new LimitSeq(this, size);
	}

	/**
	 * このリストの指定された位置の要素を返します。
	 *
	 *
	 * @param index 要素の位置
	 *
	 * @return 要素
	 */
	@Override
	public final NodeBase get(int index) {
		if(index < size - 1) return base.get(index);
		return base.drop(index);
	}

	/**
	 * このリストの要素数を返します。
	 *
	 *
	 * @return 要素数
	 */
	@Override
	public final int size() {
		return size;
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

	/**
	 * この可変長引数を別の可変長引数に変換します。
	 *
	 *
	 * @param op 写像
	 *
	 * @return 変換されたリスト
	 */
	@Override
	public ListBase map(UnaryOperator<NodeBase> op) {
		return new VariaSeq(base.map(op), size);
	}
}
