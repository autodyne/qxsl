/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.draft;

import java.time.ZonedDateTime;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.draft.Qxsl;
import qxsl.draft.Time;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;

/**
 * {@link Time}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
public final class TimeTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.TIME);

	@Test
	public void testValue() {
		final var time = ZonedDateTime.now();
		assertThat(new Time(time).value()).isEqualTo(time);
	}

	@Test
	public void testToString() {
		final var time = ZonedDateTime.now();
		assertThat(new Time(time)).hasToString(time.toString());
	}

	@Test
	public void testTime$Format() throws Exception {
		final var form = new TimeFactory();
		final var time = new Time();
		assertThat(form.decode(form.encode(time))).isEqualTo(time);
		assertThat(cache.field(form.encode(time))).isEqualTo(time);
	}
}
