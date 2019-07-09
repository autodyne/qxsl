/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import elva.ElvaLisp.ElvaRuntimeException;

/**
 * LISP処理系のマクロ式の実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/18
 */
@Params(min = 0, max = -1)
final class Syntax extends Function {
	private final Struct pars;
	private final Object body;
	private final Kernel lisp;

	/**
	 * 指定された仮引数と式と評価器でマクロ式を生成します。
	 * 仮引数の各要素がシンボルであるかの検査は行いません。
	 *
	 * @param pars 仮引数
	 * @param body 値の式
	 * @param lisp 評価器
	 */
	public Syntax(Struct pars, Object body, Kernel lisp) {
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
	public final Object apply(Struct args, Kernel eval) {
		final Nested env = new Nested(null, lisp.scope);
		if(args.size() == pars.size()) {
			for(int i = 0; i < args.size(); i++) {
				final Object par = pars.get(i);
				final Object arg = args.get(i);
				env.put((Symbol) par, arg);
			}
			return eval.eval(new Kernel(env).eval(body));
		}
		final String msg = "%s required (but %s found)";
		throw new ElvaRuntimeException(msg, pars, args);
	}
}
