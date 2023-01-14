/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.form;

import elva.lang.ElvaEval;
import elva.lang.ElvaLisp;
import elva.lang.ListBase;
import elva.lang.NativeOp;
import elva.lang.NativeOp.Args;
import elva.lang.NativeOp.Name;

import qxsl.utils.AssetUtil;

/**
 * loads the specified LISP file.
 * <pre>
 * (load string)
 * </pre>
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/26
 */
@Name("load")
@Args(min = 1, max = 1)
public final class LoadForm extends NativeOp {
	@Override
	public Object apply(ListBase args, ElvaEval eval) {
		final var name = eval.apply(args.head()).text();
		final var text = AssetUtil.root().string(name);
		return ElvaLisp.scan(text).map(eval).last();
	}
}
