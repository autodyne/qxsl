/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
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
	 * 組み込み関数を定義したスコープを構築します。
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
		this.put("progn", new $Progn());

		/*
		 * basic functions for variable assignment
		 *
		 * (set symbol-expression expression)
		 */
		this.put("set", new $Set());

		/*
		 * basic functions for list operation
		 * 
		 * (list elements)
		 * (car list)
		 * (cdr list)
		 * (length list)
		 * (member value list)
		 */
		this.put("list",   new $List());
		this.put("car",    new $Car());
		this.put("cdr",    new $Cdr());
		this.put("empty?", new $Empty$());
		this.put("length", new $Length());
		this.put("member", new $Member());

		/*
		 * basic functions for mapping operation
		 * 
		 * (mapcar sequence)
		 */
		this.put("mapcar", new $MapCar());

		/*
		 * basic functions for checking equality
		 *
		 * (equal expression expression)
		 */
		this.put("equal", new $Equal());

		/*
		 * conditional operators
		 *
		 * (if condition then else)
		 * (cond (condition statements)*)
		 */
		this.put("if",   new $If());

		/*
		 * basic functions for logical operation
		 *
		 * (and expressions)
		 * (or  expressions)
		 * (not expression)
		 */
		this.put("and", new $And());
		this.put("or",  new $Or());
		this.put("not", new $Not());

		/*
		 * basic functions for arithmetical operation
		 *
		 * (+ expressions)
		 * (- expressions)
		 * (* expressions)
		 * (/ expressions)
		 * (% expressions)
		 */
		this.put("+",   new $Add());
		this.put("-",   new $Sub());
		this.put("*",   new $Mul());
		this.put("/",   new $Div());
		this.put("mod", new $Mod());

		/*
		 * basic functions for numerical comparison
		 *
		 * (<  expressions)
		 * (>  expressions)
		 * (<= expressions)
		 * (>= expressions)
		 */
		this.put("<",  new $Lt());
		this.put(">",  new $Gt());
		this.put("<=", new $Le());
		this.put(">=", new $Ge());

		/*
		 * basic functions for string triming
		 *
		 * (str-head string-expression)
		 * (str-tail string-expression)
		 */
		this.put("str-head", new $StrHead());
		this.put("str-tail", new $StrTail());

		/*
		 * basic functions for type conversion
		 *
		 * (number string-expression)
		 * (string number-expression)
		 */
		this.put("number", new $Number());
		this.put("string", new $String());

		/*
		 * basic functions for regex matching
		 *
		 * (match pattern string)
		 */
		this.put("match", new $Match());

		/*
		 * basic functions for lambda & syntax(macro) generation
		 *
		 * (lambda (parameters) value)
		 * (syntax (parameters) macro)
		 */
		this.put("lambda", new $Lambda());
		this.put("syntax", new $Syntax());
	}

	/**
	 * LISP処理系で事前に定義されるクォート関数です。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/05/14
	 */
	@Params(min = 1, max = 1)
	private static final class $Quote implements Function {
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
	@Params(min = 1, max = 1)
	private static final class $Quasi implements Function {
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
	@Params(min = 1, max = 1)
	private static final class $Uquot implements Function {
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
	@Params(min = 1, max = -1)
	private static final class $Progn implements Function {
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
	@Params(min = 2, max = 2)
	private static final class $Set implements Function {
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
	@Params(min = 0, max = -1)
	private static final class $List implements Function {
		public Object apply(Struct args, Kernel eval) {
			ArrayList<Object> arguments = new ArrayList<>();
			for(Object v: args) arguments.add(eval.eval(v));
			return new Struct(arguments);
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
	@Params(min = 1, max = 1)
	private static final class $Car implements Function {
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
	@Params(min = 1, max = 1)
	private static final class $Cdr implements Function {
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
	@Params(min = 1, max = 1)
	private static final class $Empty$ implements Function {
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
	@Params(min = 1, max = 1)
	private static final class $Length implements Function {
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
	@Params(min = 2, max = 2)
	private static final class $Member implements Function {
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
	@Params(min = 2, max = 2)
	private static final class $MapCar implements Function {
		public Object apply(Struct args, Kernel eval) {
			final ArrayList<Object> target = new ArrayList<>();
			Function f = eval.eval(args.car(), Function.class);
			for(Object e: eval.list(args.get(1))) {
				target.add(f.apply(new Struct(e), eval));
			}
			return new Struct(target);
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
	@Params(min = 2, max = 2)
	private static final class $Equal implements Function {
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
	@Params(min = 3, max = 3)
	private static final class $If implements Function {
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
	@Params(min = 2, max = -1)
	private static final class $And implements Function {
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
	@Params(min = 2, max = -1)
	private static final class $Or implements Function {
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
	@Params(min = 1, max = 1)
	private static final class $Not implements Function {
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
	@Params(min = 2, max = -1)
	private static final class $Add implements Function {
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
	@Params(min = 2, max = -1)
	private static final class $Sub implements Function {
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
	@Params(min = 2, max = -1)
	private static final class $Mul implements Function {
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
	@Params(min = 2, max = -1)
	private static final class $Div implements Function {
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
	@Params(min = 2, max = -1)
	private static final class $Mod implements Function {
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
	@Params(min = 2, max = -1)
	private static final class $Lt implements Function {
		public Object apply(Struct args, Kernel eval) {
			final List<BigDecimal> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.real(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i).compareTo(vals.get(i + 1)) >= 0) return false;
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
	@Params(min = 2, max = -1)
	private static final class $Gt implements Function {
		public Object apply(Struct args, Kernel eval) {
			final ArrayList<BigDecimal> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.real(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i).compareTo(vals.get(i + 1)) <= 0) return false;
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
	@Params(min = 2, max = -1)
	private static final class $Le implements Function {
		public Object apply(Struct args, Kernel eval) {
			final ArrayList<BigDecimal> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.real(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i).compareTo(vals.get(i + 1)) > 0) return false;
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
	@Params(min = 2, max = -1)
	private static final class $Ge implements Function {
		public Object apply(Struct args, Kernel eval) {
			final ArrayList<BigDecimal> vals = new ArrayList<>();
			for(Object v: args) vals.add(eval.real(v));
			for(int i = 0; i < args.size() - 1; i++) {
				if(vals.get(i).compareTo(vals.get(i + 1)) < 0) return false;
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
	@Params(min = 2, max = 2)
	private static final class $StrHead implements Function {
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
	@Params(min = 2, max = 2)
	private static final class $StrTail implements Function {
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
	@Params(min = 1, max = 1)
	private static final class $Number implements Function {
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
	@Params(min = 1, max = 1)
	private static final class $String implements Function {
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
	@Params(min = 2, max = 2)
	private static final class $Match implements Function {
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
	@Params(min = 2, max = 2)
	private static final class $Lambda implements Function {
		public Object apply(Struct args, Kernel eval) {
			final Object p = args.get(0), body = args.get(1);
			Struct pars = p instanceof Struct? (Struct) p: new Struct(p);
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
	@Params(min = 2, max = 2)
	private static final class $Syntax implements Function {
		public Object apply(Struct args, Kernel eval) {
			final Object p = args.get(0), body = args.get(1);
			Struct pars = p instanceof Struct? (Struct) p: new Struct(p);
			if(!pars.stream().allMatch(Symbol.class::isInstance)) {
				final String msg = "%s contains non-name";
				throw new ElvaRuntimeException(msg, pars);
			} else return new Syntax(pars, body, eval);
		}
	}
}
