/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.qxsl;

import org.junit.Test;
import qxsl.table.Fields;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Watt}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class WattTest extends junit.framework.TestCase {
	private final Fields.Cache cache = new Fields().cache(Qxsl.WATT);
	@Test
	public void testValue() {
		assertThat(new Watt("10kW").value()).isEqualTo("10kW");
		assertThat(new Watt("10MW").value()).isEqualTo("10MW");
		assertThat(new Watt("10GW").value()).isEqualTo("10GW");
	}
	@Test
	public void testToString() {
		final String text = util.RandText.alnum(100);
		assertThat(new Watt(text)).hasToString(text);
	}
	@Test
	public void testWatt$Format() throws Exception {
		final Watt.Format form = new Watt.Format();
		final Watt watt = new Watt(util.RandText.alnum(100));
		assertThat(form.decode(form.encode(watt))).isEqualTo(watt);
		assertThat(cache.field(form.encode(watt))).isEqualTo(watt);
	}
}
