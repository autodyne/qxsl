/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.lang;

import javax.script.ScriptException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link BoolNode}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class BoolNodeTest extends Assertions {
	@Test
	public void testEquals() {
		assertThat(BoolNode.T.equals(BoolNode.T)).isTrue();
		assertThat(BoolNode.F.equals(BoolNode.F)).isTrue();
		assertThat(BoolNode.T.equals(BoolNode.F)).isFalse();
		assertThat(BoolNode.F.equals(BoolNode.T)).isFalse();
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new ElvaLisp();
		assertThat(elva.eval("#t")).isEqualTo(true);
		assertThat(elva.eval("#f")).isEqualTo(false);
	}

	@Test
	public void testToString() {
		assertThat(BoolNode.T).hasToString("#t");
		assertThat(BoolNode.F).hasToString("#f");
	}

	@Test
	public void testValue() {
		assertThat(BoolNode.T.value()).isTrue();
		assertThat(BoolNode.F.value()).isFalse();
	}
}
