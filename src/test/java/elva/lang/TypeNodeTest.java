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
 * {@link TypeNode}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class TypeNodeTest extends Assertions {
	private final TypeNode longType = new TypeNode(long.class);
	private final TypeNode charType = new TypeNode(char.class);

	@Test
	public void testEquals() {
		assertThat(longType).isEqualTo(longType);
		assertThat(charType).isEqualTo(charType);
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new ElvaLisp();
		assertThat(elva.eval("long")).isEqualTo(long.class);
		assertThat(elva.eval("char")).isEqualTo(char.class);
	}

	@Test
	public void testToString() {
		assertThat(longType).hasToString("long");
		assertThat(charType).hasToString("char");
	}

	@Test
	public void testValue() {
		assertThat(longType.value()).isEqualTo(long.class);
		assertThat(charType.value()).isEqualTo(char.class);
	}
}
