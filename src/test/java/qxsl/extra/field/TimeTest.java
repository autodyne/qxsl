/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import java.time.ZonedDateTime;
import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;

import org.junit.jupiter.api.Test;

/**
 * {@link Time}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
public final class TimeTest extends org.assertj.core.api.Assertions {
	private final Cache cache = new FieldFormats().cache(Qxsl.TIME);

	@Test
	public void testValue() {
		final ZonedDateTime time = ZonedDateTime.now();
		assertThat(new Time(time).value()).isEqualTo(time);
	}

	@Test
	public void testToString() {
		final ZonedDateTime time = ZonedDateTime.now();
		assertThat(new Time(time)).hasToString(time.toString());
	}

	@Test
	public void testTime$Format() throws Exception {
		final Time.Format form = new Time.Format();
		final Time time = new Time();
		assertThat(form.decode(form.encode(time))).isEqualTo(time);
		assertThat(cache.field(form.encode(time))).isEqualTo(time);
	}
}
