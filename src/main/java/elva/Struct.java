/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import java.util.AbstractList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * LISP処理系内部で利用される不変リストの実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/18
 */
public final class Struct extends AbstractList<Object> {
	private final List<Object> list;

	/**
	 * 空のリストを利用する際はこのインスタンスを参照します。
	 */
	public static final Struct NIL = new Struct();

	/**
	 * 指定された要素を持つリストを構築します。
	 *
	 * @param list 要素
	 */
	public Struct(List<Object> list) {
		this.list = list;
	}

	/**
	 * 指定された要素を持つリストを構築します。
	 *
	 * @param vals 要素
	 */
	public Struct(Object...vals) {
		this(Arrays.asList(vals));
	}

	/**
	 * 指定された要素を持つ引用式を構築します。
	 *
	 * @param quote 引用演算子
	 * @param value 要素
	 */
	public Struct(Quotes quote, Object value) {
		this(quote.toSymbol(), value);
	}

	/**
	 * このリストの先頭のコンスセルのCAR部を返します。
	 *
	 * @return CAR部
	 */
	public final Object car() {
		return list.get(0);
	}

	/**
	 * このリストの先頭のコンスセルのCDR部を返します。
	 *
	 * @return CDR部
	 */
	public final Struct cdr() {
		return new Struct(list.subList(1, list.size()));
	}

	/**
	 * このリストの指定された位置の要素を返します。
	 *
	 * @param index 要素の位置
	 * @return 要素
	 */
	public final Object get(int index) {
		return list.get(index);
	}

	/**
	 * このリストの要素数を返します。
	 *
	 * @return 要素数
	 */
	public final int size() {
		return list.size();
	}

	/**
	 * このリストの文字列による表現を返します。
	 *
	 * @return 文字列表現
	 */
	@Override
	public final String toString() {
		Stream<String> strm = stream().map(Objects::toString);
		return strm.collect(Collectors.joining(" ", "(", ")"));
	}
}
