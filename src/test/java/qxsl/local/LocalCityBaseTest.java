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
	private final LocalCityBase base = LocalCityBase.load("city.ja");

	@Test
	public void testGetCityByCode() {
		assertThat(base.getCityByCode("10").getName()).isEqualTo("東京都");
		assertThat(base.getCityByCode("20").getName()).isEqualTo("愛知県");
	}

	@Test
	public void testGetCityByName() {
		assertThat(base.getCityByName("広島県").getCode()).isEqualTo("35");
		assertThat(base.getCityByName("福岡県").getCode()).isEqualTo("40");
	}
}
