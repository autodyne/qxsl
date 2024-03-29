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
 * {@link AtomNode}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class AtomNodeTest extends Assertions {
	private final AtomNode FOO = new AtomNode("FOO");
	private final AtomNode foo = new AtomNode("foo");
	private final AtomNode BAR = new AtomNode("BAR");
	private final AtomNode bar = new AtomNode("bar");

	@Test
	public void testEquals() {
		assertThat(new AtomNode("MuR")).isEqualTo(new AtomNode("MuR"));
		assertThat(new AtomNode("KmR")).isEqualTo(new AtomNode("KmR"));
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new ElvaLisp();
		assertThat(elva.eval("null")).isEqualTo(null);
	}

	@Test
	public void testToString() {
		assertThat(new AtomNode("MUR")).hasToString("MUR");
		assertThat(new AtomNode("KMR")).hasToString("KMR");
	}

	@Test
	public void testValue() {
		assertThat(new AtomNode("114").value()).isEqualTo("114");
		assertThat(new AtomNode("514").value()).isEqualTo("514");
	}
}
