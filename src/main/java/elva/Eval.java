/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;
import java.util.function.UnaryOperator;

import elva.Elva.ElvaRuntimeException;

import static elva.Form.Parameters;
import static elva.Symbol.Quote.UQSPL;
import static elva.Symbol.Quote.UQUOT;
import static java.lang.Integer.MAX_VALUE;

/**
 * LISP処理系のスコープ付きの評価器の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class Eval implements UnaryOperator<Sexp> {
	private final String MIN = "%s requlres %d+ arguments";
	private final String MAX = "%s requlres ~%d arguments";
	private final Class<Parameters> PAR = Parameters.class;

	/**
	 * この評価器に関連づけられたスコープです。
	 */
	public final Nest scope;

	/**
	 * 指定されたスコープに対する評価器を構築します。
	 *
	 * @param scope 評価器のスコープ
	 */
	public Eval(Nest scope) {
		this.scope = scope;
	}

	/**
	 * 式を評価してアトムの内容を返します。
	 *
	 * @param sexp 式
	 * @return 式の内容
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Object peel(Sexp sexp) {
		return apply(sexp).value();
	}

	/**
	 * 式を評価してアトムの内容を返します。
	 * 内容がnullの場合は例外を発生します。
	 *
	 * @param sexp 式
	 * @return 式の内容
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Object some(Sexp sexp) {
		return apply(sexp).as(Object.class);
	}

	/**
	 * 式を評価して識別子を返します。
	 *
	 * @param sexp 式
	 * @return 識別子
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Symbol name(Sexp sexp) {
		return apply(sexp).as(Symbol.class);
	}

	/**
	 * 式を評価して文字列を返します。
	 *
	 * @param sexp 式
	 * @return 文字列
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final String text(Sexp sexp) {
		return apply(sexp).as(String.class);
	}

	/**
	 * 式を評価して真偽値を返します。
	 *
	 * @param sexp 式
	 * @return 真偽値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final boolean bool(Sexp sexp) {
		return apply(sexp).as(Boolean.class);
	}

	/**
	 * 式を評価して符号付き整数値を返します。
	 *
	 * @param sexp 式
	 * @return 整数値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final int sInt(Sexp sexp) {
		return real(sexp).intValueExact();
	}

	/**
	 * 式を評価して実数値を返します。
	 *
	 * @param sexp 式
	 * @return 実数値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final BigDecimal real(Sexp sexp) {
		return new Atom(apply(sexp).as(Number.class)).real();
	}

	/**
	 * 式を評価してリストを返します。
	 *
	 * @param sexp 式
	 * @return リスト
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Cons cons(Sexp sexp) {
		return Cons.cast(apply(sexp));
	}

	/**
	 * 式を評価して関数またはマクロを返します。
	 *
	 * @param sexp 式
	 * @return 関数またはマクロ
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Form form(Sexp sexp) {
		return apply(sexp).as(Form.class);
	}

	/**
	 * 指定された式の配列をリストに結合して式として評価します。
	 *
	 * @param sexp 式の要素
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Sexp apply(Object...sexp) {
		return this.apply(Cons.wrap(sexp));
	}

	/**
	 * 指定された式をアトムまたは関数適用として評価します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	@Override
	public final Sexp apply(final Sexp sexp) {
		try {
			if(Cons.NIL.equals(sexp)) return sexp;
			if(sexp instanceof Cons) return apply((Cons) sexp);
			return sexp.isSymbol()? this.scope.get(sexp): sexp;
		} catch (ElvaRuntimeException ex) {
			throw ex.add(sexp);
		} catch (RuntimeException ex) {
			throw new ElvaRuntimeException(ex).add(sexp);
		}
	}

	/**
	 * 実引数の個数を検査した後に関数を適用した値を求めます。
	 *
	 * @param cons 関数適用の式
	 * @return 演算子を適用した結果の値
	 *
	 * @throws ElvaRuntimeException 引数の個数が誤っている場合
	 */
	private final Sexp apply(final Cons cons) {
		final Form form = this.form(cons.car());
		final Cons args = cons.cdr();
		final int len = args.size();
		final var par = form.getClass().getAnnotation(PAR);
		final int min = par.min() >= 0? par.min(): MAX_VALUE;
		final int max = par.max() >= 0? par.max(): MAX_VALUE;
		if(len < min) throw new ElvaRuntimeException(MIN, form, min);
		if(len > max) throw new ElvaRuntimeException(MAX, form, max);
		return Sexp.wrap(form.apply(args, this));
	}

	/**
	 * 準引用の被引用式にて引用の部分解除を示す内部オブジェクトです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @sicne 2020/02/26
	 */
	static interface Unquote {
		public void merge(List<Sexp> seq);
		public Sexp sexp();
	}

	/**
	 * 準引用の被引用式にて通常の引用解除を示す内部オブジェクトです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @sicne 2020/02/26
	 */
	private static final class Normal implements Unquote {
		public final Sexp sexp;
		public Normal(Sexp sexp) {
			this.sexp = sexp;
		}
		@Override
		public void merge(List<Sexp> seq) {
			seq.add(this.sexp);
		}
		@Override
		public Sexp sexp() {
			return sexp;
		}
	}

	/**
	 * 準引用の被引用式にてリストの継足しを示す内部オブジェクトです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @sicne 2020/02/26
	 */
	private static final class Splice implements Unquote {
		public final Cons cons;
		public Splice(Sexp sexp) {
			this.cons = Cons.cast(sexp);
		}
		@Override
		public void merge(List<Sexp> seq) {
			cons.stream().forEach(seq::add);
		}
		@Override
		public Sexp sexp() {
			return cons;
		}
	}

	/**
	 * 指定された式を準引用の被引用式として評価した値を返します。
	 *
	 * @param quote 式
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	final Unquote quoted(Sexp quote) {
		if (quote instanceof Atom) return new Normal(quote);
		if (UQUOT.is(quote)) return new Normal(apply(quote));
		if (UQSPL.is(quote)) return new Splice(apply(quote));
		final LinkedList<Sexp> list = new LinkedList<>();
		for (Sexp e: (Cons) quote) quoted(e).merge(list);
		return new Normal(Cons.cons(list));
	}
}
