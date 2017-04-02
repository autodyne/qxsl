/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import java.util.Random;
import org.junit.Test;
import qxsl.model.Fields;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.QxmlFormat.BAND;

/**
 * {@see Band}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class BandTest extends junit.framework.TestCase {
	private static final int FREQ_LIMIT = 10_000_000;
	private final Fields fields = new Fields(BAND);
	private final Random random = new Random();
	@Test
	public void testValue() {
		final Integer freq = random.nextInt(FREQ_LIMIT);
		assertThat(new Band(freq).value(), is(freq));
	}
	@Test
	public void testToString() {
		assertThat(new Band(    1_900).toString(), is("1.9MHz"));
		assertThat(new Band(    3_500).toString(), is("3.5MHz"));
		assertThat(new Band(  430_000).toString(), is("430MHz"));
		assertThat(new Band(5_600_000).toString(), is("5.6GHz"));
	}
	@Test
	public void testToKHzString() {
		assertThat(new Band(1_900).toKHzString(), is("1900kHz"));
		assertThat(new Band(3_500).toKHzString(), is("3500kHz"));
		assertThat(new Band(7_000).toKHzString(), is("7000kHz"));
	}
	@Test
	public void testToMHzString() {
		assertThat(new Band(1_900).toMHzString(), is("1.9MHz"));
		assertThat(new Band(3_500).toMHzString(), is("3.5MHz"));
		assertThat(new Band(7_000).toMHzString(), is(  "7MHz"));
	}
	@Test
	public void testToGHzString() {
		assertThat(new Band(1_200_000).toGHzString(), is("1.2GHz"));
		assertThat(new Band(2_400_000).toGHzString(), is("2.4GHz"));
		assertThat(new Band(5_600_000).toGHzString(), is("5.6GHz"));
	}
	@Test
	public void testBand$Format() throws Exception {
		final Band.Format $form = new Band.Format();
		final Band band = new Band(random.nextInt(FREQ_LIMIT));
		assertThat($form.decode($form.encode(band)), is(band));
		assertThat(fields.cache($form.encode(band)), is(band));
	}
}
