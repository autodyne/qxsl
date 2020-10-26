/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.ScriptException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * {@link ElvaLisp}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class ElvaLispTest extends Assertions {
	private final ElvaLisp elva = new ElvaLisp();

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void test(String source) throws ScriptException {
		final var cons = elva.scan(source);
		if(cons.size() > 0) {
			assertThat(cons).hasSize(2);
			final var lhs = elva.eval(String.valueOf(cons.get(0)));
			final var rhs = elva.eval(String.valueOf(cons.get(1)));
			assertThat(NodeBase.wrap(lhs)).isEqualTo(NodeBase.wrap(rhs));
		}
	}

	public static Stream<String> testMethodSource() throws IOException {
		final var stream = ElvaLisp.class.getResourceAsStream("test.lisp");
		final var reader = new InputStreamReader(stream, UTF_8);
		try(final var br = new BufferedReader(reader)) {
			return br.lines().collect(Collectors.toList()).stream();
		}
	}
}
