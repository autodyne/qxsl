/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import javax.script.Bindings;
import javax.script.SimpleBindings;

/**
 * LISP処理系の大域変数のスコープを実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2019/07/01
 */
public final class Global extends SimpleBindings {
	/**
	 * システム関数を定義するスコープを構築します。
	 */
	public Global() {
		this.put("nil", Struct.NIL);
		this.put("null", null);
		this.put("true", true);
		this.put("false", false);

		/*
		 * basic functions for syntax operation
		 *
		 * (quote expression)
		 * (quasi expression)
		 * (uquot expression)
		 */
		this.put(Quotes.QUOTE.toString(), new $Quote());
		this.put(Quotes.QUASI.toString(), new $Quasi());
		this.put(Quotes.UQUOT.toString(), new $Uquot());

		/*
		 * basic functions for sequential processing
		 *
		 * (progn statements)
		 */
		this.put(new $Progn());

		/*
		 * basic functions for variable assignment
		 *
		 * (set symbol-expression expression)
		 */
		this.put(new $Set());

		/*
		 * basic functions for list operation
		 * 
		 * (list elements)
		 * (car list)
		 * (cdr list)
		 * (length list)
		 * (member value list)
		 */
		this.put(new $List());
		this.put(new $Car());
		this.put(new $Cdr());
		this.put(new $Empty$());
		this.put(new $Length());
		this.put(new $Member());

		/*
		 * basic functions for mapping operation
		 * 
		 * (mapcar sequence)
		 */
		this.put(new $MapCar());

		/*
		 * basic functions for checking equality
		 *
		 * (equal expression expression)
		 */
		this.put(new $Equal());

		/*
		 * conditional operators
		 *
		 * (if condition then else)
		 * (cond (condition statements)*)
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
		 * basic functions for string triming
		 *
		 * (str-head string-expression)
		 * (str-tail string-expression)
		 */
		this.put(new $StrHead());
		this.put(new $StrTail());

		/*
		 * basic functions for type conversion
		 *
		 * (number string-expression)
		 * (string number-expression)
		 */
		this.put(new $Number());
		this.put(new $String());

		/*
		 * basic functions for regex matching
		 *
		 * (match pattern string)
		 */
		this.put(new $Match());

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
	private final void put(Function func) {
		this.put(func.toString(), func);
	}

	/**
	 * LISP処理系で事前に定義されるクォート関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("quote")
	@Params(min = 1, max = 1)
	private static final class $Quote extends Function {
		public Object apply(Struct args, Kernel eval) {
			return args.car();
		}
	}

	/**
	 * LISP処理系で事前に定義される準クォート関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("quasiquote")
	@Params(min = 1, max = 1)
	private static final class $Quasi extends Function {
		public Object apply(Struct args, Kernel eval) {
			return eval.quasi(args.car());
		}
	}

	/**
	 * LISP処理系で事前に定義されるアンクォート関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("unquote")
	@Params(min = 1, max = 1)
	private static final class $Uquot extends Function {
		public Object apply(Struct args, Kernel eval) {
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
	private static final class $Progn extends Function {
		public Object apply(Struct args, Kernel eval) {
			Object last = null;
			for(Object v: args) last = eval.eval(v);
			return last;
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
	private static final class $Set extends Function {
		public Object apply(Struct args, Kernel eval) {
			final Object value = eval.eval(args.get(1));
			eval.scope.put(eval.name(args.get(0)), value);
			return value;
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
	private static final class $List extends Function {
		public Object apply(Struct args, Kernel eval) {
			final List<Object> list = new LinkedList<>();
			for(Object el: args) list.add(eval.eval(el));
			return Struct.of(list);
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
	private static final class $Car extends Function {
		public Object apply(Struct args, Kernel eval) {
			return eval.list(args.car()).car();
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
	private static final class $Cdr extends Function {
		public Object apply(Struct args, Kernel eval) {
			return eval.list(args.car()).cdr();
		}
	}

	/**
	 * LISP処理系で事前に定義されるempty?関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/17
	 */
	@Native("empty?")
	@Params(min = 1, max = 1)
	private static final class $Empty$ extends Function {
		public Object apply(Struct args, Kernel eval) {
			return eval.list(args.car()).isEmpty();
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
	private static final class $Length extends Function {
		public Object apply(Struct args, Kernel eval) {
			return BigDecimal.valueOf(eval.list(args.car()).size());
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
	private static final class $Member extends Function {
		public Object apply(Struct args, Kernel eval) {
			final Object val = eval.eval(args.car());
			final Struct list = eval.list(args.get(1));
			return list.contains(val);
		}
	}

	/**
	 * LISP処理系で事前に定義されるmapcar関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("mapcar")
	@Params(min = 2, max = 2)
	private static final class $MapCar extends Function {
		public Object apply(Struct args, Kernel eval) {
			final List<Object> target = new LinkedList<>();
			Function f = eval.eval(args.car(), Function.class);
			for(Object e: eval.list(args.cdr().car())) {
				target.add(f.apply(Struct.of(e), eval));
			}
			return Struct.of(target);
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
	private static final class $Equal extends Function {
		public Object apply(Struct args, Kernel eval) {
			final Object l = eval.eval(args.get(0));
			final Object r = eval.eval(args.get(1));
			if(l == null) return r == null;
			try {
				final BigDecimal lbd = (BigDecimal) l;
				final BigDecimal rbd = (BigDecimal) r;
				return lbd.compareTo(rbd) == 0;
			} catch (ClassCastException ex) {
				return l.equals(r);
			}
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
	@Params(min = 3, max = 3)
	private static final class $If extends Function {
		public Object apply(Struct args, Kernel eval) {
			return eval.eval(args.get(eval.bool(args.car())? 1:2));
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
	private static final class $And extends Function {
		public Object apply(Struct args, Kernel eval) {
			for(Object v: args) if(!eval.bool(v)) return false;
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
	private static final class $Or extends Function {
		public Object apply(Struct args, Kernel eval) {
			for(Object v: args) if(eval.bool(v)) return true;
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
	private static final class $Not extends Function {
		public Object apply(Struct args, Kernel eval) {
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
	private static final class $Add extends Function {
		public Object apply(Struct args, Kernel eval) {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.add(eval.real(v));
			return val;
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
	private static final class $Sub extends Function {
		public Object apply(Struct args, Kernel eval) {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.subtract(eval.real(v));
			return val;
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
	private static final class $Mul extends Function {
		public Object apply(Struct args, Kernel eval) {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.multiply(eval.real(v));
			return val;
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
	private static final class $Div extends Function {
		private final int MODE = BigDecimal.ROUND_FLOOR;
		public Object apply(Struct args, Kernel eval) {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.divide(eval.real(v), MODE);
			return val;
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
	private static final class $Mod extends Function {
		public Object apply(Struct args, Kernel eval) {
			BigDecimal val = eval.real(args.car());
			for(Object v: args.cdr()) val = val.remainder(eval.real(v));
			return val;
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
	private static final class $Lt extends Function {
		public Object apply(Struct args, Kernel eval) {
			BigDecimal left = eval.real(args.car());
			for(Object sexp: args.cdr()) {
				final BigDecimal next = eval.real(sexp);
				if(left.compareTo(next) >= 0) return false;
				left = next;
			}
			return true;
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
	private static final class $Gt extends Function {
		public Object apply(Struct args, Kernel eval) {
			BigDecimal left = eval.real(args.car());
			for(Object sexp: args.cdr()) {
				final BigDecimal next = eval.real(sexp);
				if(left.compareTo(next) <= 0) return false;
				left = next;
			}
			return true;
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
	private static final class $Le extends Function {
		public Object apply(Struct args, Kernel eval) {
			BigDecimal left = eval.real(args.car());
			for(Object sexp: args.cdr()) {
				final BigDecimal next = eval.real(sexp);
				if(left.compareTo(next) > 0) return false;
				left = next;
			}
			return true;
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
	private static final class $Ge extends Function {
		public Object apply(Struct args, Kernel eval) {
			BigDecimal left = eval.real(args.car());
			for(Object sexp: args.cdr()) {
				final BigDecimal next = eval.real(sexp);
				if(left.compareTo(next) < 0) return false;
				left = next;
			}
			return true;
		}
	}

	/**
	 * LISP処理系で事前に定義されるstr-head関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("str-head")
	@Params(min = 2, max = 2)
	private static final class $StrHead extends Function {
		public Object apply(Struct args, Kernel eval) {
			final String str = eval.text(args.car());
			BigDecimal trim = eval.real(args.get(1));
			return str.substring(0, trim.intValueExact());
		}
	}

	/**
	 * LISP処理系で事前に定義されるstr-tail関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Native("str-tail")
	@Params(min = 2, max = 2)
	private static final class $StrTail extends Function {
		public Object apply(Struct args, Kernel eval) {
			final String str = eval.text(args.car());
			BigDecimal trim = eval.real(args.get(1));
			return str.substring(trim.intValueExact());
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
	private static final class $Number extends Function {
		public Object apply(Struct args, Kernel eval) {
			return new BigDecimal(eval.text(args.car()));
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
	private static final class $String extends Function {
		public Object apply(Struct args, Kernel eval) {
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
	private static final class $Match extends Function {
		public Object apply(Struct args, Kernel eval) {
			return eval.text(args.get(1)).matches(eval.text(args.car()));
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
	private static final class $Lambda extends Function {
		public Object apply(Struct args, Kernel eval) {
			final Object p = args.get(0), body = args.get(1);
			Struct pars = p instanceof Struct? (Struct) p: Struct.of(p);
			if(!pars.stream().allMatch(Symbol.class::isInstance)) {
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
	private static final class $Syntax extends Function {
		public Object apply(Struct args, Kernel eval) {
			final Object p = args.get(0), body = args.get(1);
			Struct pars = p instanceof Struct? (Struct) p: Struct.of(p);
			if(!pars.stream().allMatch(Symbol.class::isInstance)) {
				final String msg = "%s contains non-name";
				throw new ElvaRuntimeException(msg, pars);
			} else return new Syntax(pars, body, eval);
		}
	}
}
