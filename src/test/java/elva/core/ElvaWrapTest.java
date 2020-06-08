/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import javax.script.ScriptException;
import org.junit.jupiter.api.Test;

/**
 * {@link ElvaWrap}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class ElvaWrapTest extends org.assertj.core.api.Assertions {
	private final ElvaWrap FOO = new ElvaWrap("FOO");
	private final ElvaWrap foo = new ElvaWrap("foo");
	private final ElvaWrap BAR = new ElvaWrap("BAR");
	private final ElvaWrap bar = new ElvaWrap("bar");

	@Test
	public void testValue() {
		assertThat(new ElvaWrap("114").value()).isEqualTo("114");
		assertThat(new ElvaWrap("514").value()).isEqualTo("514");
	}

	@Test
	public void testToString() {
		assertThat(new ElvaWrap("MUR")).hasToString("MUR");
		assertThat(new ElvaWrap("KMR")).hasToString("KMR");
	}

	@Test
	public void testEquals() {
		assertThat(new ElvaWrap("MuR")).isEqualTo(new ElvaWrap("MuR"));
		assertThat(new ElvaWrap("KmR")).isEqualTo(new ElvaWrap("KmR"));
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
		assertThat(elva.eval("null")).isEqualTo(null);
	}
}
