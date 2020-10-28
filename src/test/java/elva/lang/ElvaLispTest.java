/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.util.stream.Stream;
import javax.script.ScriptException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import gaas.utils.AssetUtils;

/**
 * {@link ElvaLisp}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class ElvaLispTest extends Assertions {
	private static final ElvaLisp elva = new ElvaLisp();

	private static final Stream<String> testMethodSource() {
		return AssetUtils.from(elva).lines("test.lisp");
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void test(String source) throws ScriptException {
		final var cons = elva.scan(source);
		if(cons.size() > 0) {
			assertThat(cons).hasSize(2);
			final var l = elva.eval(String.valueOf(cons.get(0)));
			final var r = elva.eval(String.valueOf(cons.get(1)));
			assertThat(NodeBase.wrap(l)).isEqualTo(NodeBase.wrap(r));
		}
	}
}
