/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Mul2}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/28
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class Mul2Test extends Assertions {
	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Mul2(text)).hasToString(text);
	}

	@Test
	public void testValue() {
		assertThat(new Mul2("100110").value()).isEqualTo("100110");
		assertThat(new Mul2("400105").value()).isEqualTo("400105");
	}
}
