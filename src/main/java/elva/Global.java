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

import elva.ElvaLisp.ElvaRuntimeException;

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
		 * (set symbol expression)
		 */
		this.put(new $Set());

		/*
		 * basic functions for list operation
		 *
		 * (cons car cdr)
		 * (list elements)
		 * (car list)
		 * (cdr list)
		 * (empty? list)
		 * (length list)
		 * (member value list)
		 */
		this.put(new $Cons());
		this.put(new $List());
		this.put(new $Car());
		this.put(new $Cdr());
		this.put(new $Empty$());
		this.put(new $Length());
		this.put(new $Member());

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
		 * (if condition then else)
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
		 * basic functions for string operation
		 *
		 * (substring string)
		 */
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
	private final void put(Function func) {
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
			final var name = eval.name(args.car());
			final var val = eval.eval(args.get(1));
			eval.scope.put(name, val);
			return val;
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
	private static final class $Cons extends Function {
		public Object apply(Struct args, Kernel eval) {
			final Object head = eval.eval(args.car());
			final Struct tail = eval.list(args.get(1));
			return new Struct(head, tail);
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
			final List<Object> seq = new LinkedList<>();
			for(Object el: args) seq.add(eval.eval(el));
			return Struct.of(seq);
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
			final Struct seq = eval.list(args.get(1));
			return seq.contains(val);
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
	 * LISP処理系で事前に定義されるnull?関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/12/06
	 */
	@Native("null?")
	@Params(min = 1, max = 1)
	private static final class $Null$ extends Function {
		public Object apply(Struct args, Kernel eval) {
			return eval.eval(args.car()) == null;
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
		private final RoundingMode MODE = RoundingMode.FLOOR;
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
	 * LISP処理系で事前に定義されるsubstring関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/12/06
	 */
	@Native("substring")
	@Params(min = 3, max = 3)
	private static final class $SubString extends Function {
		public Object apply(Struct args, Kernel eval) {
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
			final String regex = eval.text(args.car());
			final String text = eval.text(args.get(1));
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
	private static final class $Tokenize extends Function {
		public Object apply(Struct args, Kernel eval) {
			final String regex = eval.text(args.car());
			final String text = eval.text(args.get(1));
			return Struct.of((Object[]) text.split(regex));
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
			final var pars = Struct.as(args.get(0));
			final var body = args.get(1);
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
			final var pars = Struct.as(args.get(0));
			final var body = args.get(1);
			if(!pars.stream().allMatch(Symbol.class::isInstance)) {
				final String msg = "%s contains non-name";
				throw new ElvaRuntimeException(msg, pars);
			} else return new Syntax(pars, body, eval);
		}
	}
}
