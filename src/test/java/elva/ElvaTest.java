/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.*;
import java.math.BigDecimal;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.ScriptException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Elva}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 *
 */
public final class ElvaTest extends test.RandTest {
	private final Elva elva = new Elva();
	@Test
	public void testNil() throws ScriptException {
		assertThat(elva.eval("()")).isEqualTo(Cons.NIL);
	}
	@Test
	public void testNull() throws ScriptException {
		assertThat(elva.eval("null")).isNull();
	}
	@Test
	public void testReal() throws ScriptException {
		assertThat(elva.eval("114")).isEqualTo(BigDecimal.valueOf(114));
		assertThat(elva.eval("514")).isEqualTo(BigDecimal.valueOf(514));
	}
	@Test
	public void testBool() throws ScriptException {
		assertThat(elva.eval("#t")).isEqualTo(true);
		assertThat(elva.eval("#f")).isEqualTo(false);
	}
	@Test
	public void testString() throws ScriptException {
		assertThat(elva.eval("\"JA1ZLO\"")).isEqualTo("JA1ZLO");
		assertThat(elva.eval("\"JA1ZGP\"")).isEqualTo("JA1ZGP");
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void test(String source) throws ScriptException {
		final String str = String.format("(list %s)", source);
		if(!elva.scan(source).isEmpty()) {
			final var sexps = (Cons) elva.eval(str);
			assertThat(sexps).hasSize(2);
			final Object val1 = sexps.get(0);
			final Object val2 = sexps.get(1);
			assertThat(val1).isEqualTo(val2);
		}
	}
	public static Stream<String> testMethodSource() throws IOException {
		final var path = Elva.class.getResource("elva.test.lisp");
		final Reader source = new InputStreamReader(path.openStream());
		try (final BufferedReader reader = new BufferedReader(source)) {
			return reader.lines().collect(Collectors.toList()).stream();
		}
	}
}
