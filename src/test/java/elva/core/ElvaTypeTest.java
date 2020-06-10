/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import javax.script.ScriptException;
import org.junit.jupiter.api.Test;

/**
 * {@link ElvaType}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class ElvaTypeTest extends org.assertj.core.api.Assertions {
	private final ElvaType longType = new ElvaType(long.class);
	private final ElvaType charType = new ElvaType(char.class);

	@Test
	public void testValue() {
		assertThat(longType.value()).isEqualTo(long.class);
		assertThat(charType.value()).isEqualTo(char.class);
	}

	@Test
	public void testToString() {
		assertThat(longType).hasToString("(type long)");
		assertThat(charType).hasToString("(type char)");
	}

	@Test
	public void testEquals() {
		assertThat(longType).isEqualTo(longType);
		assertThat(charType).isEqualTo(charType);
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new elva.lang.ElvaRuntime();
		assertThat(elva.eval("long")).isEqualTo(long.class);
		assertThat(elva.eval("char")).isEqualTo(char.class);
	}
}
