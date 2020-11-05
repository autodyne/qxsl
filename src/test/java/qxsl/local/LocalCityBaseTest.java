/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.local;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link LocalCityBase}クラスのテスト用クラスです。
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
