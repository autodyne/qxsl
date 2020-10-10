/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.local;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link LocalCityItem}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
public final class LocalCityItemTest extends Assertions {
	private final LocalCityBase base = LocalCityBase.load("city.ja");

	/**
	 * 指定された識別子に対応する地域を返します。
	 *
	 *
	 * @param code 識別子
	 *
	 * @return 地域
	 */
	private LocalCityItem jarl(String code) {
		return base.getCityByCode(code);
	}

	@Test
	public void testToString() {
		assertThat(jarl("010102")).hasToString("北海道 札幌市北区");
		assertThat(jarl("400105")).hasToString("福岡県 福岡市西区");
	}

	@Test
	public void testGetCode() {
		assertThat(jarl("100105").getCode()).isEqualTo("100105");
		assertThat(jarl("200106").getCode()).isEqualTo("200106");
	}

	@Test
	public void testGetArea() {
		assertThat(jarl("100105").getArea()).isEqualTo("東京都");
		assertThat(jarl("200106").getArea()).isEqualTo("愛知県");
	}

	@Test
	public void testGetName() {
		assertThat(jarl("100105").getName()).isEqualTo("東京都 文京区");
		assertThat(jarl("100110").getName()).isEqualTo("東京都 目黒区");
	}
}
