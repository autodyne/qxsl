/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import org.junit.Test;
import qxsl.model.Fields;
import static org.apache.commons.lang3.RandomStringUtils.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.QxmlFormat.CITY;

/**
 * {@see City}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class CityTest extends junit.framework.TestCase {
	private final Fields fields = new Fields(CITY);
	@Test
	public void testValue() {
		final String val = randomAlphanumeric(1, 100);
		assertThat(new City(val).value(), is(val));
	}
	@Test
	public void testForName() {
		assertThat(City.forName("北海道"  ).value(), is("01"));
		assertThat(City.forName("青森県"  ).value(), is("02"));
		assertThat(City.forName("岩手県"  ).value(), is("03"));
		assertThat(City.forName("秋田県"  ).value(), is("04"));
		assertThat(City.forName("山形県"  ).value(), is("05"));
		assertThat(City.forName("宮城県"  ).value(), is("06"));
		assertThat(City.forName("福島県"  ).value(), is("07"));
		assertThat(City.forName("新潟県"  ).value(), is("08"));
		assertThat(City.forName("長野県"  ).value(), is("09"));
		assertThat(City.forName("東京都"  ).value(), is("10"));
		assertThat(City.forName("神奈川県").value(), is("11"));
		assertThat(City.forName("千葉県"  ).value(), is("12"));
		assertThat(City.forName("埼玉県"  ).value(), is("13"));
		assertThat(City.forName("茨城県"  ).value(), is("14"));
		assertThat(City.forName("栃木県"  ).value(), is("15"));
		assertThat(City.forName("群馬県"  ).value(), is("16"));
		assertThat(City.forName("山梨県"  ).value(), is("17"));
		assertThat(City.forName("静岡県"  ).value(), is("18"));
		assertThat(City.forName("岐阜県"  ).value(), is("19"));
		assertThat(City.forName("愛知県"  ).value(), is("20"));
		assertThat(City.forName("三重県"  ).value(), is("21"));
		assertThat(City.forName("京都府"  ).value(), is("22"));
		assertThat(City.forName("滋賀県"  ).value(), is("23"));
		assertThat(City.forName("奈良県"  ).value(), is("24"));
		assertThat(City.forName("大阪府"  ).value(), is("25"));
		assertThat(City.forName("和歌山県").value(), is("26"));
		assertThat(City.forName("兵庫県"  ).value(), is("27"));
		assertThat(City.forName("富山県"  ).value(), is("28"));
		assertThat(City.forName("福井県"  ).value(), is("29"));
		assertThat(City.forName("石川県"  ).value(), is("30"));
		assertThat(City.forName("岡山県"  ).value(), is("31"));
		assertThat(City.forName("島根県"  ).value(), is("32"));
		assertThat(City.forName("山口県"  ).value(), is("33"));
		assertThat(City.forName("鳥取県"  ).value(), is("34"));
		assertThat(City.forName("広島県"  ).value(), is("35"));
		assertThat(City.forName("香川県"  ).value(), is("36"));
		assertThat(City.forName("徳島県"  ).value(), is("37"));
		assertThat(City.forName("愛媛県"  ).value(), is("38"));
		assertThat(City.forName("高知県"  ).value(), is("39"));
		assertThat(City.forName("福岡県"  ).value(), is("40"));
		assertThat(City.forName("佐賀県"  ).value(), is("41"));
		assertThat(City.forName("長崎県"  ).value(), is("42"));
		assertThat(City.forName("熊本県"  ).value(), is("43"));
		assertThat(City.forName("大分県"  ).value(), is("44"));
		assertThat(City.forName("宮崎県"  ).value(), is("45"));
		assertThat(City.forName("鹿児島県").value(), is("46"));
		assertThat(City.forName("沖縄県"  ).value(), is("47"));
	}
	@Test
	public void testGetCityName() {
		for(String code: City.getAvailableCodes()) {
			assertThat(new City(code).getCityName(), is(not("")));
		}
	}
	@Test
	public void testGetPrefName() {
		for(String code: City.getAvailableCodes()) {
			assertThat(new City(code).getPrefName(), is(not("")));
		}
	}
	@Test
	public void testGetAvailableCodes() {
		assertThat(City.getAvailableCodes().size(), is(not(0)));
	}
	@Test
	public void testCity$Format() throws Exception {
		final City.Format $form = new City.Format();
		final City city = new City(randomAlphanumeric(1, 100));
		assertThat($form.decode($form.encode(city)), is(city));
		assertThat(fields.cache($form.encode(city)), is(city));
	}
}
