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
import static elva.core.ElvaName.Quote.CONST;

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
			if(sexp instanceof ElvaName) return this.locals.get(sexp);
			if(sexp instanceof ElvaList) return call((ElvaList) sexp);
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
	 * @param list 関数適用の式
	 * @return 演算子を適用した結果の値
	 *
	 * @throws ElvaRuntimeException 引数の個数が誤っている場合
	 */
	private final ElvaNode call(final ElvaList list) {
		if(list.isEmpty()) return list;
		final var head = list.head();
		final var args = list.tail();
		final var form = apply(head).form();
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
		public void expand(List<ElvaNode> seq);
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
		public void expand(List<ElvaNode> seq) {
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
		public void expand(List<ElvaNode> seq) {
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
	 * @param sexp 式
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Unquote quote(final ElvaNode sexp) {
		if(ElvaList.class.isInstance(sexp)) {
			final var list = new LinkedList<ElvaNode>();
			if(UQUOT.is(sexp)) return new Normal(apply(sexp));
			if(UQSPL.is(sexp)) return new Splice(apply(sexp));
			for(var e: (ElvaList) sexp) quote(e).expand(list);
			return new Normal(ElvaList.chain(list));
		} else return new Normal(sexp);
	}

	/**
	 * 指定された式に含まれる変数の参照を即値に置換します。
	 * 参照する変数の値が更新されても即値は更新されません。
	 *
	 * @param sexp 式
	 * @return 変換された式
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final ElvaNode compile(final ElvaNode sexp) {
		if(ElvaList.class.isInstance(sexp)) {
			if(CONST.is(sexp)) return apply(sexp);
			return sexp.list().map(this::compile);
		} else return sexp;
	}
}
