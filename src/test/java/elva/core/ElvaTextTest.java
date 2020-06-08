/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import javax.script.ScriptException;
import org.junit.jupiter.api.Test;

/**
 * {@link ElvaText}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class ElvaTextTest extends org.assertj.core.api.Assertions {
	private final ElvaText FOO = new ElvaText("FOO");
	private final ElvaText foo = new ElvaText("foo");
	private final ElvaText BAR = new ElvaText("BAR");
	private final ElvaText bar = new ElvaText("bar");

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
	public void testCompareTo() {
		assertThat(FOO.compareTo(FOO)).isZero();
		assertThat(bar.compareTo(bar)).isZero();
		assertThat(FOO.compareTo(BAR)).isPositive();
		assertThat(FOO.compareTo(bar)).isNegative();
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new elva.lang.ElvaRuntime();
		assertThat(elva.eval("\"bar\"")).isEqualTo("bar");
		assertThat(elva.eval("\"foo\"")).isEqualTo("foo");
	}
}
