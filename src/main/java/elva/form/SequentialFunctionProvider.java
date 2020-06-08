/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.form;

import elva.core.ElvaEval;
import elva.core.ElvaForm;
import elva.core.ElvaList.ChainSeq;
import elva.core.ElvaList;
import elva.core.ElvaNode;

import static elva.core.ElvaList.array;
import static elva.core.ElvaList.chain;

/**
 * LISP処理系のリスト操作の組み込み関数を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/05
 */
public final class SequentialFunctionProvider {
	/**
	 * 指定された式を評価してリストを構築します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("list")
	@ElvaForm.Parameters(min = 0, max = -1)
	public static final class $List extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return args.map(eval);
		}
	}

	/**
	 * LISP処理系で事前に定義されるcons関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/03
	 */
	@ElvaForm.Native("cons")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Cons extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var head = eval.apply(args.get(0));
			final var tail = eval.apply(args.get(1));
			return new ChainSeq(head, tail.list());
		}
	}

	/**
	 * 指定された式を評価してリストの先頭を返します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("car")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Car extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head()).list().head();
		}
	}

	/**
	 * 指定された式を評価してリストの後続を返します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("cdr")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Cdr extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head()).list().tail();
		}
	}

	/**
	 * 指定された式を評価してリストの後続の先頭を返します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@ElvaForm.Native("cadr")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Cadr extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head()).list().get(1);
		}
	}

	/**
	 * 指定された式を評価してリストの後続の後続を返します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@ElvaForm.Native("cddr")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Cddr extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head()).list().drop(2);
		}
	}

	/**
	 * 指定された式を評価してリストの指定位置の要素を返します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/05
	 */
	@ElvaForm.Native("nth")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Nth extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var idx = eval.apply(args.get(0));
			final var seq = eval.apply(args.get(1));
			return seq.list().get(idx.toInt());
		}
	}

	/**
	 * 指定された式を評価してリストの指定位置の部分を返します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/05
	 */
	@ElvaForm.Native("subseq")
	@ElvaForm.Parameters(min = 3, max = 3)
	public static final class $SubSeq extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final var list = eval.apply(args.get(0));
			final int head = eval.apply(args.get(1)).toInt();
			final int tail = eval.apply(args.get(2)).toInt();
			return list.list().take(tail).drop(head);
		}
	}

	/**
	 * 指定された式を評価してリストの長さを返します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@ElvaForm.Native("length")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Length extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return eval.apply(args.head()).list().size();
		}
	}

	/**
	 * 指定された値が評価してリストに含まれるか確認します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@ElvaForm.Native("member")
	@ElvaForm.Parameters(min = 2, max = 2)
	public static final class $Member extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			final ElvaNode val = eval.apply(args.get(0));
			final ElvaNode seq = eval.apply(args.get(1));
			return seq.list().contains(val);
		}
	}

	/**
	 * 指定された値を配列リスト構造に明示的に型変換します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/07
	 */
	@ElvaForm.Native("array")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Array extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return array(eval.apply(args.head()).iter());
		}
	}

	/**
	 * 指定された値を連鎖リスト構造に明示的に型変換します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/07
	 */
	@ElvaForm.Native("chain")
	@ElvaForm.Parameters(min = 1, max = 1)
	public static final class $Chain extends ElvaForm {
		public Object apply(ElvaList args, ElvaEval eval) {
			return chain(eval.apply(args.head()).iter());
		}
	}
}
