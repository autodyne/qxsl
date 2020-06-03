/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;
import java.util.function.UnaryOperator;

import elva.bind.*;
import elva.warn.*;

import static elva.core.ElvaName.Quote.UQSPL;
import static elva.core.ElvaName.Quote.UQUOT;

/**
 * LISP処理系のスコープ付きの評価器の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class ElvaEval implements UnaryOperator<ElvaNode> {
	/**
	 * この評価器に関連づけられたスコープです。
	 */
	public final Local locals;

	/**
	 * 指定されたスコープに対する評価器を構築します。
	 *
	 * @param locals 評価器のスコープ
	 */
	public ElvaEval(Local locals) {
		this.locals = locals;
	}

	/**
	 * 指定された評価器を親に持つ評価器を構築します。
	 *
	 * @param outer 外側の評価器
	 */
	public ElvaEval(ElvaEval outer) {
		this.locals = new Local(outer.locals);
	}

	/**
	 * 指定された式を論理式として評価します。
	 *
	 * @param sexp 式
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final boolean logic(final ElvaNode sexp) {
		return apply(sexp).bool();
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
	public final ElvaNode apply(final ElvaNode sexp) {
		try {
			if(ElvaList.NIL.equals(sexp)) return sexp;
			if(sexp instanceof ElvaName) return locals.get((ElvaName) sexp);
			if(sexp instanceof ElvaList) return this.apply((ElvaList) sexp);
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
	private final ElvaNode apply(final ElvaList cons) {
		final var form = apply(cons.head()).form();
		final var args = cons.tail();
		form.validate(args);
		return ElvaNode.wrap(form.apply(args, this));
	}

	/**
	 * 準引用の被引用式にて引用の部分解除を示す内部オブジェクトです。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	public static interface Unquote {
		public void merge(List<ElvaNode> seq);
		public ElvaNode sexp();
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
		public final ElvaNode sexp;
		public Normal(ElvaNode sexp) {
			this.sexp = sexp;
		}
		@Override
		public void merge(List<ElvaNode> seq) {
			seq.add(this.sexp);
		}
		@Override
		public ElvaNode sexp() {
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
		public final ElvaList list;
		public Splice(ElvaNode sexp) {
			this.list = ElvaList.cast(sexp);
		}
		@Override
		public void merge(List<ElvaNode> seq) {
			list.stream().forEach(seq::add);
		}
		@Override
		public ElvaNode sexp() {
			return list;
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
	public final Unquote unquote(final ElvaNode quote) {
		if(ElvaList.class.isInstance(quote)) {
			final var list = new LinkedList<ElvaNode>();
			if(UQUOT.is(quote)) return new Normal(apply(quote));
			if(UQSPL.is(quote)) return new Splice(apply(quote));
			for(var e: (ElvaList) quote) unquote(e).merge(list);
			return new Normal(ElvaList.chain(list));
		} else return new Normal(quote);
	}
}
