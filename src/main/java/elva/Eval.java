/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import elva.Elva.ElvaRuntimeException;

/**
 * LISP処理系のスコープ付きの評価器の実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/18
 */
public final class Eval {
	/**
	 * この評価器に関連づけられたスコープです。
	 */
	public final Nested scope;

	/**
	 * 指定されたスコープに対する評価器を構築します。
	 *
	 * @param scope 評価器のスコープ
	 */
	public Eval(Nested scope) {
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
		return eval(sexp).value();
	}

	/**
	 * 式を評価して識別子を返します。
	 *
	 * @param sexp 式
	 * @return 識別子
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Symbol name(Sexp sexp) {
		return eval(sexp).atom().name();
	}

	/**
	 * 式を評価して文字列を返します。
	 *
	 * @param sexp 式
	 * @return 文字列
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final String text(Sexp sexp) {
		return eval(sexp).atom().text();
	}

	/**
	 * 式を評価して真偽値を返します。
	 *
	 * @param sexp 式
	 * @return 真偽値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final boolean bool(Sexp sexp) {
		return eval(sexp).atom().bool();
	}

	/**
	 * 式を評価して実数値を返します。
	 *
	 * @param sexp 式
	 * @return 実数値
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final BigDecimal real(Sexp sexp) {
		return eval(sexp).atom().real();
	}

	/**
	 * 指定された式の値を求めます。
	 *
	 * @param sexp 式
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public Sexp eval(Sexp sexp) {
		if (Cons.NIL.equals(sexp)) return Cons.NIL;
		if (sexp instanceof Cons) return apply((Cons) sexp);
		if (sexp.isSymbol()) return scope.get(sexp.value());
		return sexp;
	}

	/**
	 * 指定された式を関数適用として評価した値を返します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	private final Sexp apply(Cons sexp) {
		try {
			final var form = eval(sexp.car()).as(Form.class);
			return Sexp.wrap(this.inspect(form, sexp.cdr()));
		} catch (ElvaRuntimeException ex) {
			throw ex.add(sexp);
		}
	}

	/**
	 * 準引用の部分解除を表現する識別子です。
	 */
	private final Atom UQUOT = new Atom(Quotes.UQUOT.toSymbol());

	/**
	 * 準引用の部分展開を表現する識別子です。
	 */
	private final Atom UQSPL = new Atom(Quotes.UQSPL.toSymbol());

	/**
	 * 準引用の被引用式にて引用の部分解除を示す内部オブジェクトです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @sicne 2020/02/26
	 */
	static interface Unquote {
		public void addAll(List<Sexp> seq);
		public Sexp sexp();
	}

	/**
	 * 準引用の被引用式にて通常の引用解除を示す内部オブジェクトです。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @sicne 2020/02/26
	 */
	private static final class Normal implements Unquote {
		public final Sexp sexp;
		public Normal(Sexp sexp) {
			this.sexp = sexp;
		}
		@Override
		public void addAll(List<Sexp> seq) {
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @sicne 2020/02/26
	 */
	private static final class Splice implements Unquote {
		public final Cons sexp;
		public Splice(Sexp sexp) {
			this.sexp = sexp.cons();
		}
		@Override
		public void addAll(List<Sexp> seq) {
			seq.addAll(this.sexp);
		}
		@Override
		public Sexp sexp() {
			return sexp;
		}
	}

	/**
	 * 指定された式を準引用の被引用式として評価した値を返します。
	 *
	 * @param quoted 式
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	final Unquote qquote(Sexp quoted) {
		if (quoted instanceof Cons) {
			final Cons seq = (Cons) quoted;
			if (Cons.NIL.equals(seq)) return new Normal(Cons.NIL);
			if (UQUOT.equals(seq.car())) return new Normal(eval(seq));
			if (UQSPL.equals(seq.car())) return new Splice(eval(seq));
			final ArrayList<Sexp> list = new ArrayList<>();
			for (Sexp sexp: seq) qquote(sexp).addAll(list);
			return new Normal(Cons.cons(list));
		} else return new Normal(quoted);
	}

	/**
	 * 実引数の個数を検査した後に演算子を適用した値を求めます。
	 *
	 * @param form 演算子
	 * @param args 引数の式
	 * @return 演算子を適用した結果の値
	 *
	 * @throws ElvaRuntimeException 引数の個数が誤っている場合
	 */
	private final Object inspect(Form form, Cons args) {
		String temp = "%s requires at-least %d and at-most %d arguments";
		final Params annon = form.getClass().getAnnotation(Params.class);
		final int len = args.size();
		final int min = annon.min() >= 0? annon.min(): Integer.MAX_VALUE;
		final int max = annon.max() >= 0? annon.max(): Integer.MAX_VALUE;
		if (min <= len && len <= max) return form.apply(args, this);
		throw new ElvaRuntimeException(temp, form, min, max);
	}
}
