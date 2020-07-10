/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaName;
import elva.core.ElvaNode;
import elva.core.ElvaWrap;
import elva.core.BaseList;
import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系の変数定義の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class AssignmentFunctionProvider {
	/**
	 * 指定された値で変数を束縛するset関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@ElvaForm.Native("set")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Set extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var key = eval.apply(args.get(0));
			final var val = eval.apply(args.get(1));
			eval.locals.put(key.name(), val);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義されるlet関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@ElvaForm.Native("let")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Let extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var setq = args.head().list();
			if(setq.size() == 2) {
				final var val = setq.get(1);
				final var key = setq.get(0).name();
				final var env = new ElvaEval(eval);
				env.locals.put(key, eval.apply(val));
				return args.tail().map(env).last();
			}
			final var msg = "%s must be (name value)";
			throw new ElvaRuntimeException(msg, setq);
		}
	}

	/**
	 * LISP処理系で事前に定義されるsymbol関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/05
	 */
	@ElvaForm.Native("symbol")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Symbol extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final ElvaNode name = eval.apply(args.head());
			return new ElvaName(name.value().toString());
		}
	}
}
