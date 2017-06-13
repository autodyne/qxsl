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
import qxsl.model.Fields;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.QxmlFormat.TIME;

/**
 * {@see Time}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class TimeTest extends junit.framework.TestCase {
	private final Fields fields = new Fields(TIME);
	@Test
	public void testValue() {
		final ZonedDateTime zdtime = ZonedDateTime.now();
		assertThat(new Time(zdtime).value(), is(zdtime));
	}
	@Test
	public void testTime$Format() throws Exception {
		final Time.Format $form = new Time.Format();
		final Time time = new Time();
		assertThat($form.decode($form.encode(time)), is(time));
		assertThat(fields.cache($form.encode(time)), is(time));
	}
}
