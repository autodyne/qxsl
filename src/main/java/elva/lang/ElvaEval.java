/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.LinkedList;
import java.util.List;
import java.util.function.UnaryOperator;

import elva.warn.ElvaRuntimeException;

import static elva.lang.NameNode.Quote.UQSPL;
import static elva.lang.NameNode.Quote.UQUOT;

/**
 * LISP処理系のスコープ付きの評価器の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/18
 */
public final class ElvaEval implements UnaryOperator<NodeBase> {
	/**
	 * この評価器に関連づけられたスコープです。
	 */
	public final ScopeMap scope;

	/**
	 * 指定されたスコープに対する評価器を構築します。
	 *
	 *
	 * @param scope 評価器のスコープ
	 */
	public ElvaEval(ScopeMap scope) {
		this.scope = scope;
	}

	/**
	 * 指定された評価器を親に持つ評価器を構築します。
	 *
	 *
	 * @param outer 外側の評価器
	 */
	public ElvaEval(ElvaEval outer) {
		this.scope = outer.scope.fork();
	}

	/**
	 * 指定された式をアトムまたは関数適用として評価します。
	 *
	 *
	 * @param sexp 式
	 *
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	@Override
	public final NodeBase apply(final NodeBase sexp) {
		try {
			if(sexp.isName()) return this.scope.get(sexp);
			return sexp.isAtom()? sexp: call(sexp.list());
		} catch (ElvaRuntimeException ex) {
			throw ex.add(sexp);
		} catch (RuntimeException ex) {
			throw new ElvaRuntimeException(ex).add(sexp);
		}
	}

	/**
	 * 実引数の個数を検査した後に関数を適用した値を求めます。
	 *
	 *
	 * @param sexp 関数適用の式
	 *
	 * @return 演算子を適用した結果の値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	private final NodeBase call(final ListBase sexp) {
		final var form = apply(sexp.head()).form();
		final var args = passArgs(sexp.tail(), form);
		return NodeBase.wrap(form.apply(args, this));
	}

	/**
	 * 指定された引数に演算子を適用できるか検査します。
	 *
	 *
	 * @param args 実引数
	 * @param form 演算子
	 *
	 * @return 可変長または固定長の引数
	 *
	 * @throws ElvaRuntimeException 引数の個数が誤っている場合
	 */
	private final ListBase passArgs(ListBase args, FormBase form) {
		final int num = args.size();
		final int min = form.getMinimumArgumentLength();
		final int max = form.getMaximumArgumentLength();
		final var MIN = "%s accepts a minimum of %d arguments";
		final var MAX = "%s accepts a maximum of %d arguments";
		if(num < min) throw new ElvaRuntimeException(MIN, form, min);
		if(num > max) throw new ElvaRuntimeException(MAX, form, max);
		if(form.isNativeOperator() || !form.isVarArgs()) return args;
		return new VariaSeq(args, min + 1);
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
		public void expand(List<NodeBase> seq);
		public NodeBase sexp();
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
		public final NodeBase sexp;

		public Normal(NodeBase sexp) {
			this.sexp = sexp;
		}

		@Override
		public void expand(List<NodeBase> seq) {
			seq.add(this.sexp);
		}

		@Override
		public NodeBase sexp() {
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
		public final ListBase list;
		public Splice(NodeBase sexp) {
			this.list = ListBase.list(sexp);
		}

		@Override
		public void expand(List<NodeBase> seq) {
			list.stream().forEach(seq::add);
		}

		@Override
		public NodeBase sexp() {
			return list;
		}
	}

	/**
	 * 指定された式を準引用の被引用式として評価した値を返します。
	 *
	 *
	 * @param sexp 式
	 *
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価により発生した例外
	 */
	public final Unquote quote(final NodeBase sexp) {
		if(sexp.isList()) {
			final var list = new LinkedList<NodeBase>();
			if(UQUOT.is(sexp)) return new Normal(apply(sexp));
			if(UQSPL.is(sexp)) return new Splice(apply(sexp));
			for(var seq: sexp.list()) quote(seq).expand(list);
			return new Normal(new CoverSeq(list));
		} else return new Normal(sexp);
	}
}
