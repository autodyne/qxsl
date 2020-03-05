/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * LISP処理系内部で利用される不変リストの実装です。
 * 
 * 
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class Cons extends Sexp implements Iterable<Sexp> {
	private final Sexp head;
	private final Cons tail;
	private final int size;

	/**
	 * 内容が空のリストを示す特別なインスタンスです。
	 */
	public static final Cons NIL = new Cons();

	/**
	 * 先頭と末尾の構成要素を指定してリストを構築します。
	 *
	 * @param head 先頭の要素
	 * @param tail 末尾の要素
	 */
	public Cons(Sexp head, Cons tail) {
		this.head = head;
		this.tail = tail == null? NIL: tail;
		this.size = this.tail.size + 1;
	}

	/**
	 * {@link #NIL}のための専用のコンストラクタです。
	 */
	private Cons() {
		this.head = null;
		this.tail = null;
		this.size = 0;
	}

	/**
	 * リスト自体を返します。
	 *
	 * @return リスト
	 */
	@Override
	public final Cons value() {
		return this;
	}

	/**
	 * 指定された値をリストに変換します。
	 *
	 * @param list 値
	 * @return リスト
	 */
	public static final Cons cast(Sexp list) {
		if(list instanceof Cons) return (Cons) list;
		return Cons.cons(list);
	}

	/**
	 * 指定された要素を持つリストを構築します。
	 *
	 * @param vals 要素
	 * @return リスト 空の場合は{@link #NIL}
	 */
	public static final Cons cons(Sexp...vals) {
		return Cons.cons(List.of(vals));
	}

	/**
	 * 指定された要素を持つリストを構築します。
	 *
	 * @param vals 要素
	 * @return リスト 空の場合は{@link #NIL}
	 */
	public static final Cons cons(List<Sexp> vals) {
		Cons cons = Cons.NIL;
		for(int i = vals.size(); i > 0; i--) {
			cons = new Cons(vals.get(i - 1), cons);
		}
		return cons;
	}

	/**
	 * 指定された要素を{@link Sexp}に包んでリストを構築します。
	 *
	 * @param vals 要素
	 * @return リスト 空の場合は{@link #NIL}
	 */
	public static final Cons wrap(Object...vals) {
		return wrap(List.of(vals));
	}

	/**
	 * 指定された要素を{@link Sexp}に包んでリストを構築します。
	 *
	 * @param vals 要素
	 * @param <E> 要素の総称型
	 * @return リスト 空の場合は{@link #NIL}
	 */
	public static final <E> Cons wrap(Collection<E> vals) {
		final var list = new LinkedList<Sexp>();
		for(var v: vals) list.add(Sexp.wrap(v));
		return cons(list);
	}

	/**
	 * このリストの先頭のコンスセルのCAR部を返します。
	 *
	 * @return CAR部
	 */
	public final Sexp car() {
		return this == NIL? NIL: head;
	}

	/**
	 * このリストの先頭のコンスセルのCDR部を返します。
	 *
	 * @return CDR部
	 */
	public final Cons cdr() {
		return this == NIL? NIL: tail;
	}

	/**
	 * このリストに対して指定された回数CDRを辿ります。
	 *
	 * @param num CDRを辿る回数
	 * @return CDR部
	 */
	public final Cons cdr(int num) {
		Cons cdr = this;
		while(num-- > 0) cdr = cdr.cdr();
		return cdr;
	}

	/**
	 * このリストの指定された位置の要素を返します。
	 *
	 * @param index 要素の位置
	 * @return 要素
	 */
	public final Sexp get(int index) {
		if(index == 0) return car();
		if(index >= 0) return tail.get(index -1);
		final String msg = String.valueOf(index);
		throw new IndexOutOfBoundsException(msg);
	}

	/**
	 * このリストの部分リストを返します。
	 *
	 * @param head 部分リストが始まる位置
	 * @param tail 部分リストが終わる位置
	 * @return 部分リスト
	 */
	public final Cons subList(int head, int tail) {
		final var vals = new LinkedList<Sexp>();
		while(head < tail) vals.add(get(head++));
		return cons(vals);
	}

	/**
	 * このリストの要素数を返します。
	 *
	 * @return 要素数
	 */
	public final int size() {
		return this.size;
	}

	/**
	 * このリストの内容をイテレータで返します。
	 *
	 * @return イテレータ
	 */
	@Override
	public final Iterator<Sexp> iterator() {
		return new Iterator$();
	}

	/**
	 * このリストの内容をストリームで返します。
	 *
	 * @return ストリーム
	 */
	public final Stream<Sexp> stream() {
		return StreamSupport.stream(spliterator(), false);
	}

	/**
	 * リストの内容を返すイテレータの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/02
	 */
	private final class Iterator$ implements Iterator<Sexp> {
		private Cons head = Cons.this;
		@Override
		public boolean hasNext() {
			return head != Cons.NIL;
		}
		@Override
		public Sexp next() {
			Sexp sexp = head.car();
			this.head = head.cdr();
			return sexp;
		}
	}

	/**
	 * このリストのハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return Objects.hash(stream().toArray());
	}

	/**
	 * このリストとオブジェクトを比較します。
	 * 同じ内容のリストであれば真を返します。
	 *
	 * @param list 比較対象のオブジェクト
	 * @return 同じ内容のリストのみtrue
	 */
	@Override
	public final boolean equals(Object list) {
		if(!Cons.class.isInstance(list)) return false;
		final var s1 = ((Cons) list).stream().toArray();
		final var s2 = ((Cons) this).stream().toArray();
		return Arrays.equals(s1, s2);
	}

	/**
	 * このリストに指定された値が含まれるか確認します。
	 *
	 * @param sexp 確認する値
	 * @return 含まれる場合にtrue
	 *
	 * @throws NullPointerException sexpがnulである場合
	 */
	public final boolean contains(Sexp sexp) {
		for(Sexp elem: this) if(sexp.equals(elem)) return true;
		return false;
	}

	/**
	 * このリストが識別子のみで構成されるか確認します。
	 *
	 * @return 識別子以外の要素を含む場合にtrue
	 */
	public final boolean containsOnlySymbols() {
		for(Sexp elem: this) if(!elem.isSymbol()) return false;
		return true;
	}

	/**
	 * このリストの内容を文字列による表現に変換します。
	 *
	 * @return 文字列表現
	 */
	@Override
	public final String toString() {
		Stream<String> strm = stream().map(Sexp::toString);
		return strm.collect(Collectors.joining(" ", "(", ")"));
	}
}
