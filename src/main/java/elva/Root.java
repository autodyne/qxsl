/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringJoiner;
import javax.script.Bindings;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

import elva.Elva.ElvaRuntimeException;

import static java.nio.charset.StandardCharsets.UTF_8;

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
		this.put("nil", Cons.NIL);
		this.put("null", null);
		this.put(Bool.T.toString(), Bool.T);
		this.put(Bool.F.toString(), Bool.F);

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
		this.put(new $Setq());
		this.put(new $Let());

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
		this.put(new $Cadr());
		this.put(new $Cddr());
		this.put(new $Nth());
		this.put(new $SubSeq());
		this.put(new $Length());
		this.put(new $Member());

		/*
		 * checking equality
		 */
		this.put(new $Equal());
		this.put(new $Nil$());
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
		this.put(new $Xor());
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
		 * regex matching
		 */
		this.put(new $Match());
		this.put(new $Split());

		/*
		 * type conversion
		 */
		this.put(new $Number());
		this.put(new $String());

		/*
		 * lambda & syntax generation
		 */
		this.put(new $Lambda());
		this.put(new $Syntax());

		/*
		 * load another program
		 */
		this.put(new $Load());
	}

	/**
	 * 指定された関数をこの環境に登録します。
	 *
	 * @param form 登録する関数
	 */
	private final void put(Form form) {
		this.put(form.toString(), form);
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
			return eval.quoted(args.car()).sexp();
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
			return eval.apply(args.car());
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
			return eval.apply(args.car());
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
			return args.map(eval).last();
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
			final var key = eval.apply(args.get(0));
			final var val = eval.apply(args.get(1));
			eval.locals.put(key.name(), val);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義されるsetq関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@Form.Native("setq")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Setq extends Form {
		public Object apply(Cons args, Eval eval) {
			final var val = eval.apply(args.get(1));
			eval.locals.put(args.car().name(), val);
			return val;
		}
	}

	/**
	 * LISP処理系で事前に定義されるlet関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@Form.Native("let")
	@Form.Parameters(min = 2, max = -1)
	private static final class $Let extends Form {
		public Object apply(Cons args, Eval eval) {
			final Eval local = new Eval(eval);
			final Atom $setq = new Atom(new $Setq());
			final Cons value = Cons.cast(args.car());
			local.apply(new Cons($setq, value));
			return args.cdr().map(local).last();
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
			return eval.apply(eval.apply(args.car()));
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
			final var head = eval.apply(args.get(0));
			final var tail = eval.apply(args.get(1));
			return new Cons(head, tail.cons());
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
			final List<Sexp> seq = new ArrayList<>();
			for(Sexp e: args) seq.add(eval.apply(e));
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
			return eval.apply(args.car()).cons().car();
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
			return eval.apply(args.car()).cons().cdr();
		}
	}

	/**
	 * LISP処理系で事前に定義されるcadr関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@Form.Native("cadr")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Cadr extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.apply(args.car()).cons().get(1);
		}
	}

	/**
	 * LISP処理系で事前に定義されるcddr関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@Form.Native("cddr")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Cddr extends Form {
		public Object apply(Cons args, Eval eval) {
			return eval.apply(args.car()).cons().cdr(2);
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
			final int idx = eval.apply(args.get(0)).ival();
			final var seq = eval.apply(args.get(1)).cons();
			return seq.get(idx);
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
			final var list = eval.apply(args.get(0)).cons();
			final int head = eval.apply(args.get(1)).ival();
			final int tail = eval.apply(args.get(2)).ival();
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
			return eval.apply(args.car()).cons().size();
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
			final Sexp val = eval.apply(args.get(0));
			final Sexp seq = eval.apply(args.get(1));
			return seq.cons().contains(val);
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
			final Sexp l = eval.apply(args.get(0));
			final Sexp r = eval.apply(args.get(1));
			return l == null? r == null: l.equals(r);
		}
	}

	/**
	 * LISP処理系で事前に定義されるnil?関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@Form.Native("nil?")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Nil$ extends Form {
		public Object apply(Cons args, Eval eval) {
			return Cons.NIL.equals(eval.apply(args.car()));
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
			return eval.apply(args.car()).value() == null;
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
			boolean cond = eval.apply(args.get(0)).bool();
			return eval.apply(args.cdr(cond? 1: 2).car());
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
			for(var v: args) if(!eval.apply(v).bool()) return false;
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
			for(var v: args) if(eval.apply(v).bool()) return true;
			return false;
		}
	}

	/**
	 * LISP処理系で事前に定義されるxor関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/10
	 */
	@Form.Native("xor")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Xor extends Form {
		public Object apply(Cons args, Eval eval) {
			final var v1 = eval.apply(args.get(0)).bool();
			final var v2 = eval.apply(args.get(1)).bool();
			return v1 ^ v2;
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
			return !eval.apply(args.car()).bool();
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
			final var seq = args.map(eval).toList(Real.class);
			final var cdr = seq.subList(1, seq.size());
			return cdr.stream().reduce(seq.get(0), Real::add);
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
			final var seq = args.map(eval).toList(Real.class);
			final var cdr = seq.subList(1, seq.size());
			return cdr.stream().reduce(seq.get(0), Real::sub);
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
			final var seq = args.map(eval).toList(Real.class);
			final var cdr = seq.subList(1, seq.size());
			return cdr.stream().reduce(seq.get(0), Real::mul);
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
			final var seq = args.map(eval).toList(Real.class);
			final var cdr = seq.subList(1, seq.size());
			return cdr.stream().reduce(seq.get(0), Real::div);
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
			final var seq = args.map(eval).toList(Real.class);
			final var cdr = seq.subList(1, seq.size());
			return cdr.stream().reduce(seq.get(0), Real::mod);
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
			final var val = eval.apply(args.car()).real();
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
			final var val = eval.apply(args.car()).real();
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
			final var val = eval.apply(args.car()).real();
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
	 * LISP処理系で事前に定義されるconcat関数です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/03/05
	 */
	@Form.Native("concat")
	@Form.Parameters(min = 2, max = 2)
	private static final class $Concat extends Form {
		public Object apply(Cons args, Eval eval) {
			final var del = eval.apply(args.get(0)).text();
			final var seq = eval.apply(args.get(1)).cons();
			final StringJoiner join = new StringJoiner(del);
			for(Sexp v: seq) join.add(String.valueOf(v.value()));
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
			final var temp = eval.apply(args.car()).text();
			final var strm = args.cdr().map(eval).stream();
			final var list = strm.map(Sexp::value);
			return String.format(temp, list.toArray());
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
			final var reg = eval.apply(args.get(0)).text();
			final var str = eval.apply(args.get(1)).text();
			return str.matches(reg);
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
			final var reg = eval.apply(args.get(0)).text();
			final var str = eval.apply(args.get(1)).text();
			return Cons.wrap(List.of(str.split(reg)));
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
			final var val = eval.apply(args.car());
			if(Real.class.isInstance(val)) return val;
			else return new BigDecimal(val.text());
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
			return String.valueOf(eval.apply(args.car()).value());
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
			final var body = Sexp.wrap(args.get(1));
			return new Lambda(pars, body, eval);
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
			final var body = Sexp.wrap(args.get(1));
			return new Syntax(pars, body, eval);
		}
	}

	/**
	 * この関数はクラスパスからプログラムを読み込みます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/26
	 */
	@Form.Native("load")
	@Form.Parameters(min = 1, max = 1)
	private static final class $Load extends Form {
		public Object apply(Cons args, Eval eval) {
			final var name = eval.apply(args.car()).text();
			final var load = Root.class.getClassLoader();
			try (var is = load.getResourceAsStream(name)) {
				var isr = new InputStreamReader(is, UTF_8);
				return new Elva().scan(isr).map(eval).last();
			} catch (IOException | ScriptException ex) {
				final String msg = "failed in loading %s: %s";
				throw new ElvaRuntimeException(msg, name, ex);
			}
		}
	}
}
