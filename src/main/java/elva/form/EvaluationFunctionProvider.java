/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import java.io.IOException;
import java.io.InputStreamReader;
import javax.script.ScriptException;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.lang.ElvaRuntime;
import elva.warn.ElvaRuntimeException;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * LISP処理系の式の評価の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class EvaluationFunctionProvider {
	/**
	 * 指定された式をそのまま引用する特殊形式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("quote")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Quote extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return args.head();
		}
	}

	/**
	 * 指定された式をそのまま引用する特殊形式です。
	 * 被引用式の内部で引用が部分的に解除されます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("quasiquote")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Quasi extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.quote(args.head()).sexp();
		}
	}

	/**
	 * 指定された式の引用を解除する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("unquote")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Uquot extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head());
		}
	}

	/**
	 * 指定された式の引用を解除する関数です。
	 * 式の結果はリストの内部に展開されます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@ElvaForm.Native("unquote-splicing")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Uqspl extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head());
		}
	}

	/**
	 * 指定された式を評価した後に再び評価します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@ElvaForm.Native("eval")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Eval extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(eval.apply(args.head()));
		}
	}

	/**
	 * この関数はクラスパスからプログラムを読み込みます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@ElvaForm.Native("load")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Load extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var name = eval.apply(args.head()).text();
			final var load = $Load.class.getClassLoader();
			try (var is = load.getResourceAsStream(name)) {
				var isr = new InputStreamReader(is, UTF_8);
				return ElvaRuntime.scan(isr).map(eval).last();
			} catch (IOException | ScriptException ex) {
				final String msg = "failed in loading %s: %s";
				throw new ElvaRuntimeException(msg, name, ex);
			}
		}
	}

	/**
	 * 指定された式を部分評価して可能なら定数に変換します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	@ElvaForm.Native("const")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Const extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head());
		}
	}

	/**
	 * 指定された式に埋め込まれた部分評価の式を評価します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	@ElvaForm.Native("compile")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Compile extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(eval.compile(args.head()));
		}
	}
}
