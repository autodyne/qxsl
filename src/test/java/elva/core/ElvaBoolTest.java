/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import javax.script.ScriptException;
import org.junit.jupiter.api.Test;

/**
 * {@link ElvaBool}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/06
 */
public final class ElvaBoolTest extends org.assertj.core.api.Assertions {
	@Test
	public void testValue() {
		assertThat(ElvaBool.T.value()).isTrue();
		assertThat(ElvaBool.F.value()).isFalse();
	}

	@Test
	public void testToString() {
		assertThat(ElvaBool.T).hasToString("#t");
		assertThat(ElvaBool.F).hasToString("#f");
	}

	@Test
	public void testEquals() {
		assertThat(ElvaBool.T.equals(ElvaBool.T)).isTrue();
		assertThat(ElvaBool.F.equals(ElvaBool.F)).isTrue();
		assertThat(ElvaBool.T.equals(ElvaBool.F)).isFalse();
		assertThat(ElvaBool.F.equals(ElvaBool.T)).isFalse();
	}

	@Test
	public void testCompareTo() {
		assertThat(ElvaBool.T.compareTo(ElvaBool.T)).isZero();
		assertThat(ElvaBool.F.compareTo(ElvaBool.F)).isZero();
		assertThat(ElvaBool.T.compareTo(ElvaBool.F)).isPositive();
		assertThat(ElvaBool.F.compareTo(ElvaBool.T)).isNegative();
	}

	@Test
	public void testEval() throws ScriptException {
		final var elva = new elva.lang.ElvaRuntime();
		assertThat(elva.eval("#t")).isEqualTo(true);
		assertThat(elva.eval("#f")).isEqualTo(false);
	}
}
