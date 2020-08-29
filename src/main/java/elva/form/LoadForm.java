/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import java.io.IOException;
import java.io.InputStreamReader;

import elva.lang.ElvaEval;
import elva.lang.ElvaLisp;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;
import elva.warn.ElvaRuntimeException;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * この関数はクラスパスからプログラムを読み込みます。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/26
 */
@Name("load")
@Args(min = 1, max = 1)
public final class LoadForm extends NativeOp {
	public Object apply(ListBase args, ElvaEval eval) {
		final var name = eval.apply(args.head()).text();
		final var load = LoadForm.class.getClassLoader();
		try (var is = load.getResourceAsStream(name)) {
			var isr = new InputStreamReader(is, UTF_8);
			return ElvaLisp.scan(isr).map(eval).last();
		} catch (IOException ex) {
			final String msg = "failed in loading %s: %s";
			throw new ElvaRuntimeException(msg, name, ex);
		}
	}
}
