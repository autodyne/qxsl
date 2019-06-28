/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import java.time.ZonedDateTime;
import org.junit.Test;
import qxsl.table.Fields;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Time}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class TimeTest extends junit.framework.TestCase {
	private final Fields.Cache cache = new Fields().cache(Qxsl.TIME);
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
