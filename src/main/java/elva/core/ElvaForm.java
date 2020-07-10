/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;

import elva.bind.Local;
import elva.warn.ElvaRuntimeException;

import static java.lang.Integer.MAX_VALUE;

/**
 * LISP処理系で定義される関数やマクロの実体を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/15
 */
public abstract class ElvaForm extends ElvaNode {
	/**
	 * 関数もしくはマクロを構築します。
	 */
	public ElvaForm() {}

	/**
	 * この式の内容を返します。
	 *
	 * @return 値
	 */
	@Override
	public final ElvaForm value() {
		return this;
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
	 * 指定された実引数と評価器に対し、返り値を求めます。
	 *
	 * @param args 実引数
	 * @param eval 評価器
	 * @return 返り値
	 */
	public abstract Object apply(BaseList args, ElvaEval eval);

	/**
	 * 指定された個数の引数でこの関数を適用できるか検査します。
	 *
	 * @param seq 実引数
	 *
	 * @throws ElvaRuntimeException 引数の個数が誤っている場合
	 */
	public final void validate(BaseList seq) {
		final var p = getClass().getAnnotation(Parameters.class);
		int min = p != null && p.min() >= 0? p.min(): MAX_VALUE;
		int max = p != null && p.max() >= 0? p.max(): MAX_VALUE;
		validate(seq, min, max);
	}

	/**
	 * 指定された個数の引数でこの関数を適用できるか検査します。
	 *
	 * @param seq 実引数
	 * @param min 引数の個数の最小値
	 * @param max 引数の個数の最大値
	 *
	 * @throws ElvaRuntimeException 引数の個数が誤っている場合
	 */
	public final void validate(BaseList seq, int min, int max) {
		final int n = seq.size();
		final String MIN = "%s requlres %d+ arguments";
		final String MAX = "%s requlres ~%d arguments";
		if(n < min) throw new ElvaRuntimeException(MIN, this, min);
		if(n > max) throw new ElvaRuntimeException(MAX, this, max);
	}

	/**
	 * LISP処理系の内部で構築されるラムダ式の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/18
	 */
	@ElvaForm.Parameters(min = 0, max = -1)
	public static final class Lambda extends ElvaForm {
		private final BaseList pars;
		private final ElvaNode body;
		private final ElvaEval lisp;

		/**
		 * 指定された仮引数と式と評価器でラムダ式を生成します。
		 * 仮引数の各要素がシンボルであるかの検査は行いません。
		 *
		 * @param pars 仮引数
		 * @param body 値の式
		 * @param lisp 評価器
		 */
		public Lambda(BaseList pars, ElvaNode body, ElvaEval lisp) {
			this.pars = pars;
			this.body = body;
			this.lisp = lisp;
			for(ElvaNode v: pars) v.ofType(ElvaName.class);
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
		@Override
		public final ElvaNode apply(BaseList args, ElvaEval eval) {
			final Local local = new Local(lisp.locals);
			if(args.size() == pars.size()) {
				for(int i = 0; i < args.size(); i++) {
					final ElvaNode param = pars.get(i);
					final ElvaNode value = eval.apply(args.get(i));
					local.put(param.ofType(ElvaName.class), value);
				}
				return new ElvaEval(local).apply(body);
			}
			final String temp = "%s required but %s found";
			throw new ElvaRuntimeException(temp, pars, args);
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
	@ElvaForm.Parameters(min = 0, max = -1)
	public static final class Syntax extends ElvaForm {
		private final BaseList pars;
		private final ElvaNode body;
		private final ElvaEval lisp;

		/**
		 * 指定された仮引数と式と評価器でマクロ式を生成します。
		 * 仮引数の各要素がシンボルであるかの検査は行いません。
		 *
		 * @param pars 仮引数
		 * @param body 値の式
		 * @param lisp 評価器
		 */
		public Syntax(BaseList pars, ElvaNode body, ElvaEval lisp) {
			this.pars = pars;
			this.body = body;
			this.lisp = lisp;
			for(ElvaNode v: pars) v.ofType(ElvaName.class);
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
		 *
		 * @throws ElvaRuntimeException 評価により発生した例外
		 */
		@Override
		public final ElvaNode apply(BaseList args, ElvaEval eval) {
			final Local local = new Local(lisp.locals);
			if(args.size() == pars.size()) {
				for(int i = 0; i < args.size(); i++) {
					final ElvaNode param = pars.get(i);
					final ElvaNode value = args.get(i);
					local.put(param.ofType(ElvaName.class), value);
				}
				return eval.apply(new ElvaEval(local).apply(body));
			}
			final String temp = "%s required but %s found";
			throw new ElvaRuntimeException(temp, pars, args);
		}
	}

	/**
	 * LISP処理系でオブジェクトの属性を呼び出す式の実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/02
	 */
	@ElvaForm.Parameters(min = 0, max = -1)
	public static final class Access extends ElvaForm {
		private final ElvaType type;
		private final ElvaName name;
		private final List<Member> list;

		/**
		 * 指定された所属と名前の属性を参照します。
		 *
		 * @param type 属性が所属する型
		 * @param name 属性の名前
		 */
		public Access(ElvaType type, ElvaName name) {
			this.type = type;
			this.name = name;
			this.list = new ArrayList<>();
			install(this.name.toString());
			if(list.isEmpty()) notFound();
		}

		/**
		 * 指定された名前の属性を待機状態に置きます。
		 *
		 * @param name 属性の名前
		 */
		private final void install(String name) {
			if(name.equals("new")) {
				final var forms = type.getConstructors();
				for(var fun: forms) list.add(new Creation(fun));
			} else {
				final var field = type.getField(name);
				final var forms = type.getMethods(name);
				if(field != null) list.add(new Variable(field));
				for(var fun: forms) list.add(new Function(fun));
			}
		}

		/**
		 * 指定された属性が利用不可能である旨を通知します。
		 *
		 * @throws ElvaRuntimeException 例外
		 */
		private final void notFound() {
			final String msg = "field %s.%s is unavailable";
			throw new ElvaRuntimeException(msg, type, name);
		}

		/**
		 * この参照式の文字列による表現を返します。
		 *
		 * @return 文字列
		 */
		@Override
		public final String toString() {
			return String.format("%s.%s", type, name);
		}

		/**
		 * メンバを呼び出す操作手順を取り決めます。
		 *
		 *
		 * @author 無線部開発班
		 *
		 * @since 2020/06/05
		 */
		private interface Member {
			/**
			 * このメンバの文字列表現を返します。
			 *
			 * @return 表現
			 */
			public String describe();

			/**
			 * 指定された引数でメンバを呼び出します。
			 *
			 * @param args 引数
			 * @return 返り値
			 *
			 * @throws Exception 何らかの例外
			 */
			public ElvaNode apply(BaseList args) throws Exception;
		}

		/**
		 * 指定されたフィールドを参照します。
		 *
		 *
		 * @author 無線部開発班
		 *
		 * @since 2020/06/02
		 */
		private final class Variable implements Member {
			private final Field form;

			/**
			 * 指定されたフィールドを参照します。
			 *
			 * @param form フィールド
			 */
			public Variable(Field form) {
				this.form = form;
			}

			/**
			 * このフィールドの詳細を返します。
			 *
			 * @return フィールドの詳細
			 */
			@Override
			public String describe() {
				return form.toGenericString();
			}

			/**
			 * 指定された実引数でフィールドを参照します。
			 *
			 * @param args 実引数
			 * @return 返り値
			 */
			@Override
			public ElvaNode apply(BaseList args) throws Exception {
				final var obj = args.head().value();
				if(args.size() == 1) return ElvaNode.wrap(form.get(obj));
				throw new ElvaRuntimeException("field requires no args");
			}
		}

		/**
		 * 指定されたメソッドを実行します。
		 *
		 *
		 * @author 無線部開発班
		 *
		 * @since 2020/06/03
		 */
		private final class Function implements Member {
			private final Method form;

			/**
			 * 指定されたメソッドを実行します。
			 *
			 * @param form メソッド
			 */
			public Function(Method form) {
				this.form = form;
			}

			/**
			 * このメソッドの詳細を返します。
			 *
			 * @return メソッドの詳細
			 */
			@Override
			public String describe() {
				return form.toGenericString();
			}

			/**
			 * 指定された実引数でメソッドを実行します。
			 *
			 * @param args 実引数
			 * @throws Exception 何らかの例外
			 */
			@Override
			public ElvaNode apply(BaseList args) throws Exception {
				final var head = args.head().value();
				final var tail = pass(form, args.tail());
				return ElvaNode.wrap(form.invoke(head, tail));
			}
		}

		/**
		 * 指定されたコンストラクタを実行します。
		 *
		 *
		 * @author 無線部開発班
		 *
		 * @since 2020/06/03
		 */
		private final class Creation implements Member {
			private final Constructor form;

			/**
			 * 指定されたコンストラクタを実行します。
			 *
			 * @param form コンストラクタ
			 */
			public Creation(Constructor form) {
				this.form = form;
			}

			/**
			 * このコンストラクタの詳細を返します。
			 *
			 * @return コンストラクタの詳細
			 */
			@Override
			public String describe() {
				return form.toGenericString();
			}

			/**
			 * 指定された実引数でコンストラクタを実行します。
			 *
			 * @param args 実引数
			 * @throws Exception 何らかの例外
			 */
			@Override
			public ElvaNode apply(BaseList args) throws Exception {
				return ElvaNode.wrap(form.newInstance(pass(form, args)));
			}
		}

		/**
		 * 指定された実引数と評価器で属性の適用を評価します。
		 *
		 * @param args 実引数
		 * @param eval 評価器
		 * @return 返り値
		 *
		 * @throws ElvaRuntimeException 評価により発生した例外
		 */
		@Override
		public final ElvaNode apply(BaseList args, ElvaEval eval) {
			final var join = new StringJoiner("\n");
			final var vals = args.map(eval);
			for(var op: this.list) try {
				return op.apply(vals);
			} catch (Exception ex) {
				join.add(op.describe());
				join.add(ex.toString());
			}
			throw new ElvaRuntimeException("candidates:%n%s", join);
		}

		/**
		 * 引数の型を検査して必要なら可変長引数にまとめます。
		 *
		 * @param info 引数の情報
		 * @param args 実引数の並び
		 * @return 実引数
		 *
		 * @throws ElvaRuntimeException 引数の型が誤っている場合
		 */
		private final Object pass(Parameter info, BaseList args) {
			final var type = info.getType();
			try {
				if(!info.isVarArgs()) return args.head().ofType(type);
				else return args.toArray(type.getComponentType());
			} catch (ClassCastException ex) {
				final var msg = "parameter %s never accepts %s";
				throw new ElvaRuntimeException(msg, info, args.head());
			} catch (ArrayStoreException ex) {
				final var msg = "parameter %s never accepts %s";
				throw new ElvaRuntimeException(msg, info, args);
			}
		}

		/**
		 * 指定された引数を所定の要素数の配列に変換します。
		 * 可変長引数の場合は所定の型の配列型にまとめます。
		 *
		 * @param form 関数
		 * @param args 実引数
		 * @return 引数の配列
		 *
		 * @throws ElvaRuntimeException 引数の個数が誤っている場合
		 * @throws ClassCastException 引数の型が誤っている場合
		 */
		private final Object[] pass(Executable form, BaseList args) {
			final int size = form.getParameterCount();
			final var pars = form.getParameters();
			final var vals = new Object[size];
			if(!form.isVarArgs()) validate(args, size, size);
			else validate(args, size - 1, Integer.MAX_VALUE);
			for(int index = 0; index < size; index++) {
				final BaseList rests = args.drop(index);
				vals[index] = pass(pars[index], rests);
			}
			return vals;
		}
	}
}
