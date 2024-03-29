/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.lang;

import java.util.stream.Stream;
import javax.script.ScriptException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.utils.AssetUtil;

/**
 * {@link ElvaLisp}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class ElvaLispTest extends Assertions {
	private static final ElvaLisp elva = new ElvaLisp();

	@ParameterizedTest
	@MethodSource("source")
	public void test(String source) throws ScriptException {
		final var cons = ElvaLisp.scan(source);
		if (cons.size() > 0) {
			assertThat(cons).hasSize(2);
			final var l = elva.eval(String.valueOf(cons.get(0)));
			final var r = elva.eval(String.valueOf(cons.get(1)));
			assertThat(NodeBase.wrap(l)).isEqualTo(NodeBase.wrap(r));
		}
	}

	private static final Stream<String> source() {
		return AssetUtil.from(elva).lines("ElvaLisp.lisp");
	}
}
