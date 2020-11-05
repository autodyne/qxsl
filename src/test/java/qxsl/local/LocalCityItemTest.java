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
	/**
	 * 指定された識別子に対応する地域を返します。
	 *
	 *
	 * @param code 識別子
	 *
	 * @return 地域
	 */
	private LocalCityItem jarl(String code) {
		return LocalCityBase.load("qxsl/local/city.ja").getByCode(code);
	}

	@Test
	public void testToString() {
		assertThat(jarl("010102")).hasToString("北海道 札幌市北区");
		assertThat(jarl("400105")).hasToString("福岡県 福岡市西区");
	}

	@Test
	public void testCode() {
		assertThat(jarl("100105").code()).isEqualTo("100105");
		assertThat(jarl("200106").code()).isEqualTo("200106");
	}

	@Test
	public void testArea() {
		assertThat(jarl("100105").area()).isEqualTo("東京都");
		assertThat(jarl("200106").area()).isEqualTo("愛知県");
	}

	@Test
	public void testName() {
		assertThat(jarl("100105").name()).isEqualTo("東京都 文京区");
		assertThat(jarl("100110").name()).isEqualTo("東京都 目黒区");
	}
}
