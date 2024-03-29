/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.local;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link LocalCityBase}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/10
 */
public final class LocalCityBaseTest extends Assertions {
	@Test
	public void testGetCityByCode() {
		final var base = LocalCityBase.load("qxsl/local/city.ja");
		assertThat(base.getByCode("10").name()).isEqualTo("東京都");
		assertThat(base.getByCode("20").name()).isEqualTo("愛知県");
	}

	@Test
	public void testGetCityByName() {
		final var base = LocalCityBase.load("qxsl/local/city.ja");
		assertThat(base.getByName("広島県").code()).isEqualTo("35");
		assertThat(base.getByName("福岡県").code()).isEqualTo("40");
	}
}
