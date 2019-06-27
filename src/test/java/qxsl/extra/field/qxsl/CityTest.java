/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.qxsl;

import java.util.Iterator;
import org.junit.Test;
import qxsl.table.Fields;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link City}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class CityTest extends junit.framework.TestCase {
	private final Fields.Cache cache = new Fields().cache(Qxsl.CITY);
	private City jarl(String code) {
		return new City("jarl", code);
	}
	@Test
	public void testValue() {
		assertThat(jarl("100110").value()).isEqualTo("jarl:100110");
		assertThat(jarl("400105").value()).isEqualTo("jarl:400105");
	}
	@Test
	public void testToString() {
		final String text = util.RandText.alnum(100);
		assertThat(jarl(text)).hasToString(text);
	}
	/**
	 * JARLのJCC/JCGコードをイテレータで返します。
	 *
	 * 
	 * @return 地域番号のイテレータ
	 */
	public static Iterator<String> testMethodSource() {
		return City.getCodes("jarl").iterator();
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testGetCityName(String code) {
		assertThat(jarl(code).getName(0)).isNotEmpty();
		assertThat(jarl(code).getName(1)).isNotEmpty();
	}
	@Test
	public void testGetCodes() {
		assertThat(City.getCodes("jarl").size()).isNotZero();
	}
	@Test
	public void testCity$Format() throws Exception {
		final City.Format form = new City.Format();
		final City city = jarl(util.RandText.alnum(100));
		assertThat(form.decode(form.encode(city))).isEqualTo(city);
		assertThat(cache.field(form.encode(city))).isEqualTo(city);
	}
}
