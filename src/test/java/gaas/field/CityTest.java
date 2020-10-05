/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.field;

import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;

/**
 * {@link City}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
public final class CityTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.CITY);
	private City jarl(String code) {
		return City.forCode("jarl", code);
	}

	@Test
	public void testValue() {
		assertThat(jarl("100110").value()).isEqualTo("jarl:100110");
		assertThat(jarl("400105").value()).isEqualTo("jarl:400105");
	}

	@Test
	public void testGetCode() {
		assertThat(jarl("100105").getCode()).isEqualTo("100105");
		assertThat(jarl("200106").getCode()).isEqualTo("200106");
	}

	@Test
	public void testToString() {
		assertThat(jarl("100110")).hasToString("jarl:100110");
		assertThat(jarl("400105")).hasToString("jarl:400105");
	}

	/**
	 * JARLのJCC/JCGに含まれる全ての地域を返します。
	 *
	 *
	 * @return 地域の集合
	 */
	public static List<City> testMethodSource() {
		return City.all("jarl");
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetFullPath(City city) {
		assertThat(city.getFullPath()).hasSizeBetween(1,2);
		assertThat(city.getFullPath()).doesNotContain("");
		assertThat(city.getFullPath()).doesNotContainNull();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetFullName(City city) {
		var name = String.join("", city.getFullPath());
		assertThat(city.getFullName()).isEqualTo(name);
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testForCode(City city) {
		assertThat(jarl(city.value().split(":")[1])).isEqualTo(city);
	}

	@Test
	public void testAll() {
		assertThat(City.all("jarl")).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testCity$Format(City city) throws Exception {
		final var form = new City.Factory();
		assertThat(form.decode(form.encode(city))).isEqualTo(city);
		assertThat(cache.field(form.encode(city))).isEqualTo(city);
	}
}
