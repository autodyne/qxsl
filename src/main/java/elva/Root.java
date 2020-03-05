/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.StringJoiner;
import javax.script.Bindings;
import javax.script.SimpleBindings;

import elva.Elva.ElvaRuntimeException;
import static java.math.MathContext.DECIMAL64;

/**
 * LISP処理系の大域変数のスコープを実装します。
 * 
 * 
 * @author 無線部開発班
 *
 * @since 2019/07/01
 */
final class Root extends SimpleBindings {
	/**
	 * システム関数を定義するスコープを構築します。
	 */
	public Root() {
		this.put("#t", true);
		this.put("#f", false);
		this.put("nil", Cons.NIL);
		this.put("null", null);

		/*
		 * syntax operation
		 */
		this.put(new $Quote());
		this.put(new $Quasi());
		this.put(new $Uquot());
		this.put(new $Uqspl());

		/*
		 * sequential processing
		 */
		this.put(new $Progn());

		/*
		 * variable assignment
		 */
		this.put(new $Set());

		/*
		 * evaluation
		 */
		this.put(new $Eval());

		/*
		 * list operation
		 */
		this.put(new $Cons());
		this.put(new $List());
		this.put(new $Car());
		this.put(new $Cdr());
		this.put(new $Nth());
		this.put(new $SubSeq());
		this.put(new $Length());
		this.put(new $Member());

		/*
		 * list reduction
		 */
		this.put(new $Every());
		this.put(new $Some());

		/*
		 * checking equality
		 */
		this.put(new $Equal());
		this.put(new $Null$());

		/*
		 * conditional operators
		 */
		this.put(new $If());

		/*
		 * logical operation
		 */
		this.put(new $And());
		this.put(new $Or());
		this.put(new $Not());

		/*
		 * arithmetical operation
		 */
		this.put(new $Add());
		this.put(new $Sub());
		this.put(new $Mul());
		this.put(new $Div());
		this.put(new $Mod());

		/*
		 * round operation
		 */
		this.put(new $Ceiling());
		this.put(new $Floor());
		this.put(new $Round());

		/*
		 * numerical comparison
		 */
		this.put(new $Lt());
		this.put(new $Gt());
		this.put(new $Le());
		this.put(new $Ge());

		/*
		 * string operation
		 */
		this.put(new $Concat());
		this.put(new $Format());

		/*
		 * type conversion
		 */
		this.put(new $Number());
		this.put(new $String());

		/*
		 * regex matching
		 */
		this.put(new $Match());
		this.put(new $Split());

		/*
		 * lambda & syntax generation
		 */
		this.put(new $Lambda());
		this.put(new $Syntax());
	}

	/**
	 * 指定された関数をこの環境に登録します。
	 *
	 * @param func 登録する関数
	 */
	private final void put(Form func) {
		this.put(func.toString(), func);
	}

	/**
	 * 指定された名前に束縛された値を返します。
	 *
	 * @param name 名前
	 * @return 束縛された値
	 */
	@Override
	public final Object get(Object name) {
		final String key = name.toString();
		if(containsKey(key)) return super.get(key);
		final String msg = "unknown symbol '%s'";
		throw new ElvaRuntimeException(msg, name);
	}

	/**
	 * LISP処理系で事前に定義される引用関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("quote")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Quote extends Form {
		public Object apply(Cons args, Eval eval) {
			return args.car();
		}
	}

	/**
	 * LISP処理系で事前に定義される準引用関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("quasiquote")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Quasi extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.qquote(args.car()).sexp();
		}
	}

	/**
	 * LISP処理系で事前に定義される引用解除の関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("unquote")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Uquot extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car());
		}
	}

	/**
	 * LISP処理系で事前に定義される引用解除の関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("unquote-splicing")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Uqspl extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car());
		}
	}

	/**
	 * LISP処理系で事前に定義されるprogn関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@Form.Native("progn")
	@Form.Parameters(min = 1, max = -1)
	private static final class $Progn extends Form {
		public Object apply(Cons args, Eval eval) {
			final var list = args.stream().map(eval::eval);
			final var last = list.reduce((head, tl) -> tl);
			return last.orElse(Cons.NIL);
		}
	}

	/**
	 * LISP処理系で事前に定義されるset関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@Form.Native("set")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Set extends Form {
		public Object apply(Cons args, Eval eval) {
			final var name = eval.name(args.car());
			final var val = eval.eval(args.get(1));
			eval.scope.put(name, val);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義されるeval関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("eval")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Eval extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(eval.eval(args.car()));
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
	@Form.Native("cons")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Cons extends Form {
		public Object apply(Cons args, Eval eval) {
			final var head = eval.eval(args.get(0));
			final var tail = eval.cons(args.get(1));
			return new Cons(head, tail);
		}
	}

	/**
	 * LISP処理系で事前に定義されるlist関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("list")
	@Form.Parameters(min = 0, max = -1)
	private static final class $List extends Form {
		public Object apply(Cons args, Eval eval) {
			final List<Sexp> seq = new LinkedList<>();
			for(Sexp el: args) seq.add(eval.eval(el));
			return Cons.cons(seq);
		}
	}

	/**
	 * LISP処理系で事前に定義されるcar関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("car")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Car extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.cons(args.car()).car();
		}
	}

	/**
	 * LISP処理系で事前に定義されるcdr関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("cdr")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Cdr extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.cons(args.car()).cdr();
		}
	}

	/**
	 * LISP処理系で事前に定義されるnth関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/05
	 */
	@Form.Native("nth")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Nth extends Form {
		public Object apply(Cons args, Eval eval) {
			final var idx = eval.real(args.get(0));
			final var seq = eval.cons(args.get(1));
			return seq.get(idx.intValueExact());
		}
	}

	/**
	 * LISP処理系で事前に定義されるsubseq関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/05
	 */
	@Form.Native("subseq")
	@Form.Parameters(min = 3, max = 3)
	private static final class $SubSeq extends Form {
		public Object apply(Cons args, Eval eval) {
			final var list = eval.cons(args.get(0));
			final var arg1 = eval.real(args.get(1));
			final var arg2 = eval.real(args.get(2));
			final int head = arg1.intValueExact();
			final int tail = arg2.intValueExact();
			return list.subList(head, tail);
		}
	}

	/**
	 * LISP処理系で事前に定義されるlength関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("length")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Length extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.cons(args.car()).size();
		}
	}

	/**
	 * LISP処理系で事前に定義されるmember関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@Form.Native("member")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Member extends Form {
		public Object apply(Cons args, Eval eval) {
			final Sexp val = eval.eval(args.get(0));
			final Cons seq = eval.cons(args.get(1));
			return seq.contains(val);
		}
	}

	/**
	 * LISP処理系で事前に定義されevery関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("every")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Every extends Form {
		public Object apply(Cons args, Eval eval) {
			for(Sexp val: eval.cons(args.car())) {
				if(!val.as(Boolean.class)) return false;
			}
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義されsome関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("some")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Some extends Form {
		public Object apply(Cons args, Eval eval) {
			for(Sexp val: eval.cons(args.car())) {
				if(val.as(Boolean.class)) return true;
			}
			return false;
		}
	}

	/**
	 * LISP処理系で事前に定義されるequal関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@Form.Native("equal")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Equal extends Form {
		public Object apply(Cons args, Eval eval) {
			final Sexp l = eval.eval(args.get(0));
			final Sexp r = eval.eval(args.get(1));
			return l == null? r == null: l.equals(r);
		}
	}

	/**
	 * LISP処理系で事前に定義されるnull?関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/12/06
	 */
	@Form.Native("null?")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Null$ extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car()).value() == null;
		}
	}

	/**
	 * LISP処理系で事前に定義されるif関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@Form.Native("if")
	@Form.Parameters(min = 2, max = 3)
	private static final class $If extends Form {
		public Object apply(Cons args, Eval eval) {
			int cond = eval.bool(args.car())? 1: 2;
			return eval.eval(args.cdr(cond).car());
		}
	}

	/**
	 * LISP処理系で事前に定義されるand関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@Form.Native("and")
	@Form.Parameters(min = 2, max = -1)
	private static final class $And extends Form {
		public Object apply(Cons args, Eval eval) {
			for(var v: args) if(!eval.bool(v)) return false;
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義されるor関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/27
	 */
	@Form.Native("or")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Or extends Form {
		public Object apply(Cons args, Eval eval) {
			for(var v: args) if(eval.bool(v)) return true;
			return false;
		}
	}

	/**
	 * LISP処理系で事前に定義されるand関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@Form.Native("not")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Not extends Form {
		public Object apply(Cons args, Eval eval) {
			return !eval.bool(args.car());
		}
	}

	/**
	 * LISP処理系で事前に定義される加算演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/19
	 */
	@Form.Native("+")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Add extends Form {
		public Object apply(Cons args, Eval eval) {
			final BigDecimal car = eval.real(args.get(0));
			var cdr = args.cdr().stream().map(eval::real);
			return cdr.reduce(car, BigDecimal::add);
		}
	}

	/**
	 * LISP処理系で事前に定義される減算演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/19
	 */
	@Form.Native("-")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Sub extends Form {
		public Object apply(Cons args, Eval eval) {
			final BigDecimal car = eval.real(args.get(0));
			var cdr = args.cdr().stream().map(eval::real);
			return cdr.reduce(car, BigDecimal::subtract);
		}
	}

	/**
	 * LISP処理系で事前に定義される乗算演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/19
	 */
	@Form.Native("*")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Mul extends Form {
		public Object apply(Cons args, Eval eval) {
			final BigDecimal car = eval.real(args.get(0));
			var cdr = args.cdr().stream().map(eval::real);
			return cdr.reduce(car, BigDecimal::multiply);
		}
	}

	/**
	 * LISP処理系で事前に定義される除算演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/19
	 */
	@Form.Native("/")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Div extends Form {
		public Object apply(Cons args, Eval eval) {
			final BigDecimal car = eval.real(args.get(0));
			var cdr = args.cdr().stream().map(eval::real);
			return cdr.reduce(car, (l, r) -> l.divide(r, DECIMAL64));
		}
	}

	/**
	 * LISP処理系で事前に定義される剰余演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/18
	 */
	@Form.Native("mod")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Mod extends Form {
		public Object apply(Cons args, Eval eval) {
			final BigDecimal car = eval.real(args.get(0));
			var cdr = args.cdr().stream().map(eval::real);
			return cdr.reduce(car, BigDecimal::remainder);
		}
	}

	/**
	 * LISP処理系で事前に定義される切り上げ演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/28
	 */
	@Form.Native("ceiling")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Ceiling extends Form {
		public Object apply(Cons args, Eval eval) {
			final BigDecimal val = eval.real(args.car());
			return val.setScale(0, RoundingMode.CEILING);
		}
	}

	/**
	 * LISP処理系で事前に定義される切り捨て演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/28
	 */
	@Form.Native("floor")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Floor extends Form {
		public Object apply(Cons args, Eval eval) {
			final BigDecimal val = eval.real(args.car());
			return val.setScale(0, RoundingMode.FLOOR);
		}
	}

	/**
	 * LISP処理系で事前に定義される四捨五入演算子です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/28
	 */
	@Form.Native("round")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Round extends Form {
		public Object apply(Cons args, Eval eval) {
			final BigDecimal val = eval.real(args.car());
			return val.setScale(0, RoundingMode.HALF_UP);
		}
	}

	/**
	 * LISP処理系で事前に定義される不等号&lt;の関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/17
	 */
	@Form.Native("<")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Lt extends Form {
		public Object apply(Cons args, Eval eval) {
			var prev = eval.real(args.car());
			var flag = true;
			for(var sexp: args.cdr()) {
				final BigDecimal next = eval.real(sexp);
				flag &= prev.compareTo(prev = next) < 0;
			}
			return flag;
		}
	}

	/**
	 * LISP処理系で事前に定義される不等号&gt;の関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/17
	 */
	@Form.Native(">")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Gt extends Form {
		public Object apply(Cons args, Eval eval) {
			var prev = eval.real(args.car());
			var flag = true;
			for(var sexp: args.cdr()) {
				final BigDecimal next = eval.real(sexp);
				flag &= prev.compareTo(prev = next) > 0;
			}
			return flag;
		}
	}

	/**
	 * LISP処理系で事前に定義される不等号&lt;=の関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/17
	 */
	@Form.Native("<=")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Le extends Form {
		public Object apply(Cons args, Eval eval) {
			var prev = eval.real(args.car());
			var flag = true;
			for(var sexp: args.cdr()) {
				final BigDecimal next = eval.real(sexp);
				flag &= prev.compareTo(prev = next) <= 0;
			}
			return flag;
		}
	}

	/**
	 * LISP処理系で事前に定義される不等号&gt;=の関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/03/17
	 */
	@Form.Native(">=")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Ge extends Form {
		public Object apply(Cons args, Eval eval) {
			var prev = eval.real(args.car());
			var flag = true;
			for(var sexp: args.cdr()) {
				final BigDecimal next = eval.real(sexp);
				flag &= prev.compareTo(prev = next) >= 0;
			}
			return flag;
		}
	}

	/**
	 * LISP処理系で事前に定義されるconcat関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/05
	 */
	@Form.Native("concat")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Concat extends Form {
		public Object apply(Cons args, Eval eval) {
			final var join = new StringJoiner("");
			for(Sexp sexp: eval.cons(args.car())) {
				join.add(String.valueOf(sexp.value()));
			}
			return join.toString();
		}
	}

	/**
	 * LISP処理系で事前に定義されるformat関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("format")
	@Form.Parameters(min = 1, max = -1)
	private static final class $Format extends Form {
		public Object apply(Cons args, Eval eval) {
			final var temp = eval.text(args.car());
			final var strm = args.cdr(1).stream();
			final var vals = strm.map(eval::peel);
			return String.format(temp, vals.toArray());
		}
	}

	/**
	 * LISP処理系で事前に定義されるnumber関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/06/30
	 */
	@Form.Native("number")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Number extends Form {
		public Object apply(Cons args, Eval eval) {
			final var val = eval.some(args.car());
			return new BigDecimal(val.toString());
		}
	}

	/**
	 * LISP処理系で事前に定義されるstring関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/06/30
	 */
	@Form.Native("string")
	@Form.Parameters(min = 1, max = 1)
	private static final class $String extends Form {
		public Object apply(Cons args, Eval eval) {
			return String.valueOf(eval.eval(args.car()));
		}
	}

	/**
	 * LISP処理系で事前に定義されるmatch関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/16
	 */
	@Form.Native("match")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Match extends Form {
		public Object apply(Cons args, Eval eval) {
			final var regex = eval.text(args.car());
			final var text = eval.text(args.get(1));
			return text.matches(regex);
		}
	}

	/**
	 * LISP処理系で事前に定義されるsplit関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/12/06
	 */
	@Form.Native("split")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Split extends Form {
		public Object apply(Cons args, Eval eval) {
			final String regex = eval.text(args.car());
			final String text = eval.text(args.get(1));
			return Cons.wrap((Object[]) text.split(regex));
		}
	}

	/**
	 * LISP処理系で事前に定義されるlambda関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("lambda")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Lambda extends Form {
		public Object apply(Cons args, Eval eval) {
			final var pars = Cons.cast(args.get(0));
			final var body = args.get(1);
			if(!pars.containsOnlySymbols()) {
				final String msg = "%s contains non-name";
				throw new ElvaRuntimeException(msg, pars);
			} else return new Lambda(pars, body, eval);
		}
	}

	/**
	 * LISP処理系で事前に定義されるsyntax関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/14
	 */
	@Form.Native("syntax")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Syntax extends Form {
		public Object apply(Cons args, Eval eval) {
			final var pars = Cons.cast(args.get(0));
			final var body = args.get(1);
			if(!pars.containsOnlySymbols()) {
				final String msg = "%s contains non-name";
				throw new ElvaRuntimeException(msg, pars);
			} else return new Syntax(pars, body, eval);
		}
	}
}
