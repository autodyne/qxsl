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
 * {@link TextNode}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class TextNodeTest extends Assertions {
	private final TextNode FOO = new TextNode("FOO");
	private final TextNode foo = new TextNode("foo");
	private final TextNode BAR = new TextNode("BAR");
	private final TextNode bar = new TextNode("bar");

	@Test
	public void testEquals() {
		assertThat(FOO).isEqualTo(FOO);
		assertThat(bar).isEqualTo(bar);
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new elva.lang.ElvaLisp();
		assertThat(elva.eval("\"bar\"")).isEqualTo("bar");
		assertThat(elva.eval("\"foo\"")).isEqualTo("foo");
	}

	@Test
	public void testToString() {
		assertThat(FOO).hasToString("\"FOO\"");
		assertThat(foo).hasToString("\"foo\"");
		assertThat(BAR).hasToString("\"BAR\"");
		assertThat(bar).hasToString("\"bar\"");
	}

	@Test
	public void testValue() {
		assertThat(foo.value()).isEqualTo("foo");
		assertThat(BAR.value()).isEqualTo("BAR");
	}
}
