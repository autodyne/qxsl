/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import javax.script.ScriptException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link NameNode}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class NameNodeTest extends Assertions {
	private final NameNode FOO = new NameNode("FOO");
	private final NameNode foo = new NameNode("foo");
	private final NameNode BAR = new NameNode("BAR");
	private final NameNode bar = new NameNode("bar");

	@Test
	public void testEquals() {
		assertThat(FOO).isEqualTo(FOO);
		assertThat(bar).isEqualTo(bar);
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new ElvaLisp();
		assertThat(elva.eval("'bar")).isEqualTo(bar);
		assertThat(elva.eval("'foo")).isEqualTo(foo);
	}

	@Test
	public void testToString() {
		assertThat(FOO).hasToString("FOO");
		assertThat(foo).hasToString("foo");
		assertThat(BAR).hasToString("BAR");
		assertThat(bar).hasToString("bar");
	}

	@Test
	public void testValue() {
		assertThat(foo.value()).isEqualTo(foo);
		assertThat(BAR.value()).isEqualTo(BAR);
	}
}
