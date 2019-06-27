/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.jarl;

import org.junit.Test;
import qxsl.table.Fields;

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
	private final Fields.Cache cache = new Fields().cache(Jarl.CITY);
	@Test
	public void testValue() {
		assertThat(new City("100110").value()).isEqualTo("100110");
		assertThat(new City("400105").value()).isEqualTo("400105");
	}
	@Test
	public void testToString() {
		final String text = util.RandText.alnum(100);
		assertThat(new City(text)).hasToString(text);
	}
	@Test
	public void testForName() {
		assertThat(City.forName("北海道"  )).hasToString("01");
		assertThat(City.forName("青森県"  )).hasToString("02");
		assertThat(City.forName("岩手県"  )).hasToString("03");
		assertThat(City.forName("秋田県"  )).hasToString("04");
		assertThat(City.forName("山形県"  )).hasToString("05");
		assertThat(City.forName("宮城県"  )).hasToString("06");
		assertThat(City.forName("福島県"  )).hasToString("07");
		assertThat(City.forName("新潟県"  )).hasToString("08");
		assertThat(City.forName("長野県"  )).hasToString("09");
		assertThat(City.forName("東京都"  )).hasToString("10");
		assertThat(City.forName("神奈川県")).hasToString("11");
		assertThat(City.forName("千葉県"  )).hasToString("12");
		assertThat(City.forName("埼玉県"  )).hasToString("13");
		assertThat(City.forName("茨城県"  )).hasToString("14");
		assertThat(City.forName("栃木県"  )).hasToString("15");
		assertThat(City.forName("群馬県"  )).hasToString("16");
		assertThat(City.forName("山梨県"  )).hasToString("17");
		assertThat(City.forName("静岡県"  )).hasToString("18");
		assertThat(City.forName("岐阜県"  )).hasToString("19");
		assertThat(City.forName("愛知県"  )).hasToString("20");
		assertThat(City.forName("三重県"  )).hasToString("21");
		assertThat(City.forName("京都府"  )).hasToString("22");
		assertThat(City.forName("滋賀県"  )).hasToString("23");
		assertThat(City.forName("奈良県"  )).hasToString("24");
		assertThat(City.forName("大阪府"  )).hasToString("25");
		assertThat(City.forName("和歌山県")).hasToString("26");
		assertThat(City.forName("兵庫県"  )).hasToString("27");
		assertThat(City.forName("富山県"  )).hasToString("28");
		assertThat(City.forName("福井県"  )).hasToString("29");
		assertThat(City.forName("石川県"  )).hasToString("30");
		assertThat(City.forName("岡山県"  )).hasToString("31");
		assertThat(City.forName("島根県"  )).hasToString("32");
		assertThat(City.forName("山口県"  )).hasToString("33");
		assertThat(City.forName("鳥取県"  )).hasToString("34");
		assertThat(City.forName("広島県"  )).hasToString("35");
		assertThat(City.forName("香川県"  )).hasToString("36");
		assertThat(City.forName("徳島県"  )).hasToString("37");
		assertThat(City.forName("愛媛県"  )).hasToString("38");
		assertThat(City.forName("高知県"  )).hasToString("39");
		assertThat(City.forName("福岡県"  )).hasToString("40");
		assertThat(City.forName("佐賀県"  )).hasToString("41");
		assertThat(City.forName("長崎県"  )).hasToString("42");
		assertThat(City.forName("熊本県"  )).hasToString("43");
		assertThat(City.forName("大分県"  )).hasToString("44");
		assertThat(City.forName("宮崎県"  )).hasToString("45");
		assertThat(City.forName("鹿児島県")).hasToString("46");
		assertThat(City.forName("沖縄県"  )).hasToString("47");
	}
	@Test
	public void testGetCityName() {
		for(String code: City.getAvailableCodes()) {
			assertThat(new City(code).getCityName()).isNotEmpty();
		}
	}
	@Test
	public void testGetPrefName() {
		for(String code: City.getAvailableCodes()) {
			assertThat(new City(code).getPrefName()).isNotEmpty();
		}
	}
	@Test
	public void testGetAvailableCodes() {
		assertThat(City.getAvailableCodes().size()).isNotZero();
	}
	@Test
	public void testCity$Format() throws Exception {
		final City.Format form = new City.Format();
		final City city = new City(util.RandText.alnum(100));
		assertThat(form.decode(form.encode(city))).isEqualTo(city);
		assertThat(cache.field(form.encode(city))).isEqualTo(city);
	}
}
