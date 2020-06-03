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
	 * 指定された式を評価した後に再び評価します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@ElvaForm.Native("eval")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $ElvaEval extends ElvaForm {
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
}
