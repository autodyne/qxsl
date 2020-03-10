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
import static elva.Name.Quote.UQSPL;
import static elva.Name.Quote.UQUOT;
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
	public final Nest locals;

	/**
	 * 指定されたスコープに対する評価器を構築します。
	 *
	 * @param locals 評価器のスコープ
	 */
	public Eval(Nest locals) {
		this.locals = locals;
	}

	/**
	 * 指定された評価器を親に持つ評価器を構築します。
	 *
	 * @param outer 外側の評価器
	 */
	public Eval(Eval outer) {
		this.locals = new Nest(outer.locals);
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
			if(sexp instanceof Name) return locals.get((Name) sexp);
			if(sexp instanceof Cons) return this.apply((Cons) sexp);
			return sexp;
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
		final Form form = apply(cons.car()).form();
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
	 * @since 2020/02/26
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
	 * @since 2020/02/26
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
	 * @since 2020/02/26
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
		if (Cons.class.isInstance(quote)) {
			if (UQUOT.is(quote)) return new Normal(apply(quote));
			if (UQSPL.is(quote)) return new Splice(apply(quote));
			final LinkedList<Sexp> list = new LinkedList<>();
			for (Sexp e: (Cons) quote) quoted(e).merge(list);
			return new Normal(Cons.cons(list));
		} else return new Normal(quote);
	}
}
