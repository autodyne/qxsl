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
import javax.script.Bindings;
import javax.script.SimpleBindings;

import elva.Elva.ElvaRuntimeException;
import static java.math.MathContext.DECIMAL64;

/**
 * LISP処理系の大域変数のスコープを実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2019/07/01
 */
final class Global extends SimpleBindings {
	/**
	 * システム関数を定義するスコープを構築します。
	 */
	public Global() {
		this.put("#t", true);
		this.put("#f", false);
		this.put("nil", Cons.NIL);
		this.put("null", null);

		/*
		 * basic functions for syntax operation
		 *
		 * (quote expression)
		 * (quasi expression)
		 * (unquote expression)
		 * (unquote-splicing expression)
		 */
		this.put(new $Quote());
		this.put(new $Quasi());
		this.put(new $Uquot());
		this.put(new $Uqspl());

		/*
		 * basic functions for sequential processing
		 *
		 * (progn statements)
		 */
		this.put(new $Progn());

		/*
		 * basic functions for variable assignment
		 *
		 * (set symbol expression)
		 */
		this.put(new $Set());

		/*
		 * basic functions for evaluation
		 *
		 * (eval expression)
		 */
		this.put(new $Eval());

		/*
		 * basic functions for list operation
		 *
		 * (cons car cdr)
		 * (list elements)
		 * (car list)
		 * (cdr list)
		 * (length list)
		 * (member value list)
		 */
		this.put(new $Cons());
		this.put(new $List());
		this.put(new $Car());
		this.put(new $Cdr());
		this.put(new $Length());
		this.put(new $Member());

		/*
		 * basic functions for list reduction
		 *
		 * (every list)
		 * (some  list)
		 */
		this.put(new $Every());
		this.put(new $Some());

		/*
		 * basic functions for checking equality
		 *
		 * (equal expression expression)
		 * (null? expression)
		 */
		this.put(new $Equal());
		this.put(new $Null$());

		/*
		 * conditional operators
		 *
		 * (if condition then [else])
		 */
		this.put(new $If());

		/*
		 * basic functions for logical operation
		 *
		 * (and expressions)
		 * (or  expressions)
		 * (not expression)
		 */
		this.put(new $And());
		this.put(new $Or());
		this.put(new $Not());

		/*
		 * basic functions for arithmetical operation
		 *
		 * (+ expressions)
		 * (- expressions)
		 * (* expressions)
		 * (/ expressions)
		 * (% expressions)
		 */
		this.put(new $Add());
		this.put(new $Sub());
		this.put(new $Mul());
		this.put(new $Div());
		this.put(new $Mod());

		/*
		 * basic functions for round operation
		 *
		 * (ceiling expressions)
		 * (floor expressions)
		 * (round expressions)
		 */
		this.put(new $Ceiling());
		this.put(new $Floor());
		this.put(new $Round());

		/*
		 * basic functions for numerical comparison
		 *
		 * (<  expressions)
		 * (>  expressions)
		 * (<= expressions)
		 * (>= expressions)
		 */
		this.put(new $Lt());
		this.put(new $Gt());
		this.put(new $Le());
		this.put(new $Ge());

		/*
		 * basic functions for string operation
		 *
		 * (format string)
		 * (substring string)
		 */
		this.put(new $Format());
		this.put(new $SubString());

		/*
		 * basic functions for type conversion
		 *
		 * (number string)
		 * (string number)
		 */
		this.put(new $Number());
		this.put(new $String());

		/*
		 * basic functions for regex matching
		 *
		 * (match pattern string)
		 * (tokenize pattern string)
		 */
		this.put(new $Match());
		this.put(new $Tokenize());

		/*
		 * basic functions for lambda & syntax(macro) generation
		 *
		 * (lambda (parameters) value)
		 * (syntax (parameters) macro)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("quote")
	@Params(min = 1, max = 1)
	private static final class $Quote extends Form {
		public Object apply(Cons args, Eval eval) {
			return args.car();
		}
	}

	/**
	 * LISP処理系で事前に定義される準引用関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("quasiquote")
	@Params(min = 1, max = 1)
	private static final class $Quasi extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.qquote(args.car()).sexp();
		}
	}

	/**
	 * LISP処理系で事前に定義される引用解除の関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("unquote")
	@Params(min = 1, max = 1)
	private static final class $Uquot extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car());
		}
	}

	/**
	 * LISP処理系で事前に定義される引用解除の関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/26
	 */
	@Native("unquote-splicing")
	@Params(min = 1, max = 1)
	private static final class $Uqspl extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car());
		}
	}

	/**
	 * LISP処理系で事前に定義されるprogn関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	@Native("progn")
	@Params(min = 1, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	@Native("set")
	@Params(min = 2, max = 2)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/26
	 */
	@Native("eval")
	@Params(min = 1, max = 1)
	private static final class $Eval extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(eval.eval(args.car()));
		}
	}

	/**
	 * LISP処理系で事前に定義されるcons関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/07/03
	 */
	@Native("cons")
	@Params(min = 2, max = 2)
	private static final class $Cons extends Form {
		public Object apply(Cons args, Eval eval) {
			final var head = eval.eval(args.get(0));
			final var tail = eval.eval(args.get(1));
			return new Cons(head, tail.cons());
		}
	}

	/**
	 * LISP処理系で事前に定義されるlist関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("list")
	@Params(min = 0, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("car")
	@Params(min = 1, max = 1)
	private static final class $Car extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car()).cons().car();
		}
	}

	/**
	 * LISP処理系で事前に定義されるcdr関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("cdr")
	@Params(min = 1, max = 1)
	private static final class $Cdr extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car()).cons().cdr();
		}
	}

	/**
	 * LISP処理系で事前に定義されるlength関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("length")
	@Params(min = 1, max = 1)
	private static final class $Length extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car()).cons().size();
		}
	}

	/**
	 * LISP処理系で事前に定義されるmember関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	@Native("member")
	@Params(min = 2, max = 2)
	private static final class $Member extends Form {
		public Object apply(Cons args, Eval eval) {
			final Sexp val = eval.eval(args.get(0));
			final Sexp seq = eval.eval(args.get(1));
			return seq.cons().contains(val);
		}
	}

	/**
	 * LISP処理系で事前に定義されevery関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/26
	 */
	@Native("every")
	@Params(min = 1, max = 1)
	private static final class $Every extends Form {
		public Object apply(Cons args, Eval eval) {
			final Cons seq = eval.eval(args.car()).cons();
			return seq.stream().allMatch(Atom.TRUE::equals);
		}
	}

	/**
	 * LISP処理系で事前に定義されsome関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/26
	 */
	@Native("some")
	@Params(min = 1, max = 1)
	private static final class $Some extends Form {
		public Object apply(Cons args, Eval eval) {
			final Cons seq = eval.eval(args.car()).cons();
			return seq.stream().anyMatch(Atom.TRUE::equals);
		}
	}

	/**
	 * LISP処理系で事前に定義されるequal関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	@Native("equal")
	@Params(min = 2, max = 2)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/12/06
	 */
	@Native("null?")
	@Params(min = 1, max = 1)
	private static final class $Null$ extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.eval(args.car()).value() == null;
		}
	}

	/**
	 * LISP処理系で事前に定義されるif関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	@Native("if")
	@Params(min = 2, max = 3)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	@Native("and")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/27
	 */
	@Native("or")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Native("not")
	@Params(min = 1, max = 1)
	private static final class $Not extends Form {
		public Object apply(Cons args, Eval eval) {
			return !eval.bool(args.car());
		}
	}

	/**
	 * LISP処理系で事前に定義される加算演算子です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	@Native("+")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	@Native("-")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	@Native("*")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/19
	 */
	@Native("/")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/18
	 */
	@Native("mod")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/28
	 */
	@Native("ceiling")
	@Params(min = 1, max = 1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/28
	 */
	@Native("floor")
	@Params(min = 1, max = 1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/28
	 */
	@Native("round")
	@Params(min = 1, max = 1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/17
	 */
	@Native("<")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/17
	 */
	@Native(">")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/17
	 */
	@Native("<=")
	@Params(min = 2, max = -1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/03/17
	 */
	@Native(">=")
	@Params(min = 2, max = -1)
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
	 * LISP処理系で事前に定義されるformat関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/26
	 */
	@Native("format")
	@Params(min = 1, max = -1)
	private static final class $Format extends Form {
		public Object apply(Cons args, Eval eval) {
			final var temp = eval.text(args.car());
			final var strm = args.cdr(1).stream();
			final var vals = strm.map(eval::peel);
			return String.format(temp, vals.toArray());
		}
	}

	/**
	 * LISP処理系で事前に定義されるsubstring関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/12/06
	 */
	@Native("substring")
	@Params(min = 3, max = 3)
	private static final class $SubString extends Form {
		public Object apply(Cons args, Eval eval) {
			final String str = eval.text(args.car());
			final int head = eval.real(args.get(1)).intValueExact();
			final int tail = eval.real(args.get(2)).intValueExact();
			return str.substring(head, tail);
		}
	}

	/**
	 * LISP処理系で事前に定義されるnumber関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/06/30
	 */
	@Native("number")
	@Params(min = 1, max = 1)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/06/30
	 */
	@Native("string")
	@Params(min = 1, max = 1)
	private static final class $String extends Form {
		public Object apply(Cons args, Eval eval) {
			return String.valueOf(eval.eval(args.car()));
		}
	}

	/**
	 * LISP処理系で事前に定義されるmatch関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/16
	 */
	@Native("match")
	@Params(min = 2, max = 2)
	private static final class $Match extends Form {
		public Object apply(Cons args, Eval eval) {
			final var regex = eval.text(args.car());
			final var text = eval.text(args.get(1));
			return text.matches(regex);
		}
	}

	/**
	 * LISP処理系で事前に定義されるtokenize関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/12/06
	 */
	@Native("tokenize")
	@Params(min = 2, max = 2)
	private static final class $Tokenize extends Form {
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("lambda")
	@Params(min = 2, max = 2)
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
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("syntax")
	@Params(min = 2, max = 2)
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
