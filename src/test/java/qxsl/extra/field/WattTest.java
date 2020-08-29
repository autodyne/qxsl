/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Watt}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class WattTest extends org.assertj.core.api.Assertions {
	private final Cache cache = new FieldFormats().cache(Qxsl.WATT);

	@Test
	public void testValue() {
		assertThat(new Watt("10kW").value()).isEqualTo("10kW");
		assertThat(new Watt("10MW").value()).isEqualTo("10MW");
		assertThat(new Watt("10GW").value()).isEqualTo("10GW");
	}

	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Watt(text)).hasToString(text);
	}

	@Test
	public void testWatt$Format(@RandomString String text) throws Exception {
		final var form = new Watt.Format();
		final var watt = new Watt(text);
		assertThat(form.decode(form.encode(watt))).isEqualTo(watt);
		assertThat(cache.field(form.encode(watt))).isEqualTo(watt);
	}
}
