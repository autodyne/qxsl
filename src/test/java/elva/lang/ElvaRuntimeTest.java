/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.io.*;
import java.math.BigDecimal;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.ScriptException;

import elva.core.ElvaList;
import elva.core.ElvaNode;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * {@link ElvaRuntime}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class ElvaRuntimeTest extends org.assertj.core.api.Assertions {
	private final ElvaRuntime elva = new ElvaRuntime();

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void test(String source) throws ScriptException {
		final ElvaList cons = elva.scan(source);
		if(cons.size() > 0) {
			assertThat(cons).hasSize(2);
			final var lhs = elva.eval(String.valueOf(cons.get(0)));
			final var rhs = elva.eval(String.valueOf(cons.get(1)));
			assertThat(ElvaNode.wrap(lhs)).isEqualTo(ElvaNode.wrap(rhs));
		}
	}

	public static Stream<String> testMethodSource() throws IOException {
		final var path = ElvaRuntime.class.getResource("test.lisp");
		final Reader source = new InputStreamReader(path.openStream());
		try (final BufferedReader reader = new BufferedReader(source)) {
			return reader.lines().collect(Collectors.toList()).stream();
		}
	}
}
