/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import javax.script.ScriptException;

import org.junit.jupiter.api.Test;

/**
 * {@link TextNode}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class TextNodeTest extends org.assertj.core.api.Assertions {
	private final TextNode FOO = new TextNode("FOO");
	private final TextNode foo = new TextNode("foo");
	private final TextNode BAR = new TextNode("BAR");
	private final TextNode bar = new TextNode("bar");

	@Test
	public void testValue() {
		assertThat(foo.value()).isEqualTo("foo");
		assertThat(BAR.value()).isEqualTo("BAR");
	}

	@Test
	public void testToString() {
		assertThat(FOO).hasToString("\"FOO\"");
		assertThat(foo).hasToString("\"foo\"");
		assertThat(BAR).hasToString("\"BAR\"");
		assertThat(bar).hasToString("\"bar\"");
	}

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
}
