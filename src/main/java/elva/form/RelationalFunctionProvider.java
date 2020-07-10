/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import java.util.Objects;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList;
import elva.core.ElvaNode;
import elva.core.BaseList;

/**
 * LISP処理系の比較演算の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class RelationalFunctionProvider {
	/**
	 * 左側の値が右側の値未満であるか確認する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/17
	 */
	@ElvaForm.Native("<")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Lt extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var list = args.map(eval);
			for(int i = 1; i < list.size(); i++) {
				final var l = list.get(i - 1).real();
				final var r = list.get(i + 0).real();
				if(l.compareTo(r) >= 0) return false;
			}
			return true;
		}
	}

	/**
	 * 右側の値が左側の値未満であるか確認する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/17
	 */
	@ElvaForm.Native(">")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Gt extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var list = args.map(eval);
			for(int i = 1; i < list.size(); i++) {
				final var l = list.get(i - 1).real();
				final var r = list.get(i + 0).real();
				if(l.compareTo(r) <= 0) return false;
			}
			return true;
		}
	}

	/**
	 * 右側の値が左側の値以上であるか確認する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/17
	 */
	@ElvaForm.Native("<=")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Le extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var list = args.map(eval);
			for(int i = 1; i < list.size(); i++) {
				final var l = list.get(i - 1).real();
				final var r = list.get(i + 0).real();
				if(l.compareTo(r) > 0) return false;
			}
			return true;
		}
	}

	/**
	 * 左側の値が右側の値以上であるか確認する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/17
	 */
	@ElvaForm.Native(">=")
	@ElvaForm.Parameters(min = 2, max = -1)
	public static final class $Ge extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final var list = args.map(eval);
			for(int i = 1; i < list.size(); i++) {
				final var l = list.get(i - 1).real();
				final var r = list.get(i + 0).real();
				if(l.compareTo(r) < 0) return false;
			}
			return true;
		}
	}

	/**
	 * 指定された式を評価して値が空リストか確認する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@ElvaForm.Native("nil?")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Nil$ extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			return ElvaList.NIL.equals(eval.apply(args.head()));
		}
	}

	/**
	 * 指定された式を評価して値が空の参照か確認する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/12/06
	 */
	@ElvaForm.Native("null?")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Null$ extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			return eval.apply(args.head()).value() == null;
		}
	}

	/**
	 * 指定された式を評価して両辺の等値性を確認する関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@ElvaForm.Native("equal")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Equal extends ElvaForm {
		public Object apply(BaseList args, ElvaEval eval) {
			final ElvaNode l = eval.apply(args.get(0));
			final ElvaNode r = eval.apply(args.get(1));
			return Objects.equals(l, r);
		}
	}
}
