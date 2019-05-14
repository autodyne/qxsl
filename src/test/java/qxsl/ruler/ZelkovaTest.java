/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import javax.script.ScriptException;
import javax.script.SimpleScriptContext;
import org.junit.Test;
import static java.util.Arrays.asList;
import static javax.script.ScriptContext.ENGINE_SCOPE;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import static qxsl.ruler.Zelkova.List;

/**
 * {@see Zelkova}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class ZelkovaTest extends junit.framework.TestCase {
	private final Zelkova lisp = new Zelkova();
	private final SimpleScriptContext c;
	public ZelkovaTest() {
		c = new SimpleScriptContext();
		c.setBindings(lisp.createBindings(), ENGINE_SCOPE);
	}
	@Test
	public void testNil() throws ScriptException {
		assertThat(lisp.eval("nil", c), is(lisp.eval("()", c)));
	}
	@Test
	public void testNull() throws ScriptException {
		assertThat(lisp.eval("null", c), is(nullValue()));
	}
	@Test
	public void testQuote() throws ScriptException {
		assertThat(lisp.eval("'114514", c), is(114514));
		assertThat(lisp.eval("'\"ABC\"", c), is("ABC"));
	}
	@Test
	public void testQuasi() throws ScriptException {
		assertThat(lisp.eval("`(3 6)", c), is(new List(3, 6)));
	}
	@Test
	public void testUquot() throws ScriptException {
		assertThat(lisp.eval("`(3 ,(+ 3 3) 4)", c), is(new List(3, 6, 4)));
	}
	@Test
	public void testProgn() throws ScriptException {
		assertThat(lisp.eval("(progn (set 'foo 13) '53)", c), is(53));
		assertThat(lisp.eval("(progn (set 'bar 97) foo)", c), is(13));
	}
	@Test
	public void testSet() throws ScriptException {
		assertThat(lisp.eval("(set 'MUR (equal 364 364))", c), is(true));
		assertThat(lisp.eval("(if MUR 1919810 889464)", c), is(1919810));
	}
	@Test
	public void testList() throws ScriptException {
		assertThat(lisp.eval("(list 8 1 0)", c), is(asList(8, 1, 0)));
		assertThat(lisp.eval("(list 3 6 4)", c), is(asList(3, 6, 4)));
	}
	@Test
	public void testCar() throws ScriptException {
		assertThat(lisp.eval("(car (list 114 514 810))", c), is(114));
	}
	@Test
	public void testCdr() throws ScriptException {
		assertThat(lisp.eval("(cdr (list 1 23))", c), is(asList(23)));
	}
	@Test
	public void testLength() throws ScriptException {
		assertThat(lisp.eval("(length (list 12345))", c), is(1));
		assertThat(lisp.eval("(length (list 12 45))", c), is(2));
		assertThat(lisp.eval("(length (list 1 2 3))", c), is(3));
	}
	@Test
	public void testMember() throws ScriptException {
		assertThat(lisp.eval("(member 'A (list 'A 'B 'C))", c), is(true));
		assertThat(lisp.eval("(member 'B (list 'A 'B 'C))", c), is(true));
		assertThat(lisp.eval("(member 'C (list 'A 'B 'C))", c), is(true));
		assertThat(lisp.eval("(not (member 'D (list 'A)))", c), is(true));
	}
	@Test
	public void testEqual() throws ScriptException {
		assertThat(lisp.eval("(equal 6 (car (list 6 2)))", c), is(true));
		assertThat(lisp.eval("(equal (list 1) (list 2))", c), is(false));
	}
	@Test
	public void testIf() throws ScriptException {
		assertThat(lisp.eval("(if (equal 1 1) 114 514)", c), is(114));
		assertThat(lisp.eval("(if (equal 1 2) 114 514)", c), is(514));
	}
	@Test
	public void testAnd() throws ScriptException {
		assertThat(lisp.eval("(and false false)", c), is(false));
		assertThat(lisp.eval("(and false true )", c), is(false));
		assertThat(lisp.eval("(and true  false)", c), is(false));
		assertThat(lisp.eval("(and true  true )", c), is(true));
	}
	@Test
	public void testOr() throws ScriptException {
		assertThat(lisp.eval("(or false false)", c), is(false));
		assertThat(lisp.eval("(or false true )", c), is(true));
		assertThat(lisp.eval("(or true  false)", c), is(true));
		assertThat(lisp.eval("(or true  true )", c), is(true));
	}
	@Test
	public void testNot() throws ScriptException {
		assertThat(lisp.eval("(not (not (not true)))", c), is(false));
	}
	@Test
	public void testAdd() throws ScriptException {
		assertThat(lisp.eval("(+ 1 1 4 (+ 5 1 4))", c), is(16));
		assertThat(lisp.eval("(+ (+ 3 6 4) 3 6 4)", c), is(26));
	}
	@Test
	public void testSub() throws ScriptException {
		assertThat(lisp.eval("(- 1 1 4 (- 5 1 4))", c), is(-4));
		assertThat(lisp.eval("(- (+ 8 8 9) 4 6 4)", c), is(11));
	}
	@Test
	public void testMul() throws ScriptException {
		assertThat(lisp.eval("(* 3 6 4 (* 3 6 4))", c), is(5184));
		assertThat(lisp.eval("(* (+ 8 8 9) 4 6 4)", c), is(2400));
	}
	@Test
	public void testDiv() throws ScriptException {
		assertThat(lisp.eval("(/ 364364 1919)", c), is(189));
		assertThat(lisp.eval("(/ 889464 1919)", c), is(463));
	}
	@Test
	public void testLt() throws ScriptException {
		assertThat(lisp.eval("(< 1 2 3 (if false 2 4) 5)", c), is(true));
		assertThat(lisp.eval("(< 1 2 3 (if true 2 4) 5)", c), is(false));
	}
	@Test
	public void testGt() throws ScriptException {
		assertThat(lisp.eval("(> 5 4 (if false 5 3) 2 1)", c), is(true));
		assertThat(lisp.eval("(> 5 4 (if true 5 3) 2 1)", c), is(false));
	}
	@Test
	public void testLe() throws ScriptException {
		assertThat(lisp.eval("(<= 1 3 3 (if false 2 4) 5)", c), is(true));
		assertThat(lisp.eval("(<= 1 3 3 (if true 2 4) 5)", c), is(false));
	}
	@Test
	public void testGe() throws ScriptException {
		assertThat(lisp.eval("(>= 5 3 (if false 5 3) 2 1)", c), is(true));
		assertThat(lisp.eval("(>= 5 3 (if true 5 3) 2 1)", c), is(false));
	}
	@Test
	public void testStrHead() throws ScriptException {
		assertThat(lisp.eval("(str-head \"HELLO\")", c), is("H"));
		assertThat(lisp.eval("(str-head \"WORLD\")", c), is("W"));
	}
	@Test
	public void testStrTail() throws ScriptException {
		assertThat(lisp.eval("(str-tail \"hello\")", c), is("ello"));
		assertThat(lisp.eval("(str-tail \"world\")", c), is("orld"));
	}
	@Test
	public void testLambda() throws ScriptException {
		lisp.eval("(set 'EQUAL (lambda (a b) (equal a b)))", c);
		assertThat(lisp.eval("(EQUAL 1919 1919)", c), is(true));
		assertThat(lisp.eval("(EQUAL 1919 810)", c), is(false));
	}
	@Test
	public void testSyntax() throws ScriptException {
		lisp.eval("(set 'quartz (syntax (arg) (list quote arg)))", c);
		final Object q = lisp.eval("(quartz (list 1919 810 364))", c);
		assertThat(String.valueOf(q), is("(list 1919 810 364)"));
	}
}
