/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiFunction;

import elva.Elva.ElvaRuntimeException;

/**
 * LISP処理系でシンボルが参照する関数やマクロの実体を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
public abstract class Form implements BiFunction<Cons, Eval, Object> {
	/**
	 * 関数もしくはマクロを構築します。
	 */
	public Form() {}

	/**
	 * 指定された実引数と評価器に対し、返り値を求めます。
	 *
	 * @param args 実引数
	 * @param eval 評価器
	 * @return 返り値
	 */
	public abstract Object apply(Cons args, Eval eval);

	/**
	 * この関数の名前を注釈{@link Native}経由で返します。
	 *
	 * @return 関数の名前
	 */
	@Override
	public String toString() {
		Native annon = getClass().getAnnotation(Native.class);
		return annon != null? annon.value(): super.toString();
	}

	/**
	 * 関数定義の引数の個数を指定する注釈型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/17
	 */
	@Target(ElementType.TYPE)
	@Retention(RetentionPolicy.RUNTIME)
	public static @interface Parameters {
		/**
		 * 評価前の引数の最小限の個数です。
		 *
		 * @return 引数の個数
		 */
		public int min();
		/**
		 * 評価前の引数の最大限の個数です。
		 *
		 * @return 引数の個数
		 */
		public int max();
	}

	/**
	 * LISP処理系のシステム関数であることを示す注釈型です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/02
	 */
	@Target(ElementType.TYPE)
	@Retention(RetentionPolicy.RUNTIME)
	public static @interface Native {
		/**
		 * このシステム関数の名前です。
		 *
		 * @return 関数の名前
		 */
		public String value();
	}

	/**
	 * LISP処理系の内部で構築されるラムダ式の実装です。
	 * 
	 * 
	 * @author 無線部開発班
	 *
	 * @since 2017/02/18
	 */
	@Parameters(min = 0, max = -1)
	public static final class Lambda extends Form {
		private final Cons pars;
		private final Sexp body;
		private final Eval lisp;

		/**
		 * 指定された仮引数と式と評価器でラムダ式を生成します。
		 * 仮引数の各要素がシンボルであるかの検査は行いません。
		 *
		 * @param pars 仮引数
		 * @param body 値の式
		 * @param lisp 評価器
		 */
		public Lambda(Cons pars, Sexp body, Eval lisp) {
			this.pars = pars;
			this.body = body;
			this.lisp = lisp;
		}

		/**
		 * このラムダ式の文字列による表現を返します。
		 *
		 * @return 文字列
		 */
		@Override
		public final String toString() {
			return String.format("(lambda %s %s)", pars, body);
		}

		/**
		 * 指定された実引数と評価器でラムダ式の適用を評価します。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 *
		 * @throws ElvaRuntimeException 評価により発生した例外
		 */
		public final Sexp apply(Cons args, Eval eval) {
			final Nest env = new Nest(null, lisp.scope);
			if(args.size() == pars.size()) {
				for(int i = 0; i < args.size(); i++) {
					final Sexp par = pars.get(i);
					final Sexp arg = args.get(i);
					env.put(par.as(Symbol.class), eval.eval(arg));
				}
				return new Eval(env).eval(body);
			}
			final String msg = "%s required, but %s found";
			throw new ElvaRuntimeException(msg, pars, args);
		}
	}

	/**
	 * LISP処理系の内部で構築されるマクロ式の実装です。
	 * 
	 * 
	 * @author 無線部開発班
	 *
	 * @since 2017/02/18
	 */
	@Parameters(min = 0, max = -1)
	public static final class Syntax extends Form {
		private final Cons pars;
		private final Sexp body;
		private final Eval lisp;

		/**
		 * 指定された仮引数と式と評価器でマクロ式を生成します。
		 * 仮引数の各要素がシンボルであるかの検査は行いません。
		 *
		 * @param pars 仮引数
		 * @param body 値の式
		 * @param lisp 評価器
		 */
		public Syntax(Cons pars, Sexp body, Eval lisp) {
			this.pars = pars;
			this.body = body;
			this.lisp = lisp;
		}

		/**
		 * このマクロ式の文字列による表現を返します。
		 *
		 * @return 文字列
		 */
		@Override
		public final String toString() {
			return String.format("(syntax %s %s)", pars, body);
		}

		/**
		 * 指定された実引数と評価器でマクロ式の適用を評価します。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 * @throws ElvaRuntimeException 評価により発生した例外
		 */
		public final Sexp apply(Cons args, Eval eval) {
			final Nest env = new Nest(null, lisp.scope);
			if(args.size() == pars.size()) {
				for(int i = 0; i < args.size(); i++) {
					final Sexp par = pars.get(i);
					final Sexp arg = args.get(i);
					env.put(par.as(Symbol.class), arg);
				}
				return eval.eval(new Eval(env).eval(body));
			}
			final String msg = "%s required, but %s found";
			throw new ElvaRuntimeException(msg, pars, args);
		}
	}
}
