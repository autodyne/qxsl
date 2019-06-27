/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.qxsl;

import java.util.Random;
import org.junit.Test;
import qxsl.table.Fields;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Band}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class BandTest extends junit.framework.TestCase {
	private final Fields.Cache cache = new Fields().cache(Qxsl.BAND);
	private final Random random = new Random();
	@Test
	public void testValue() {
		final Integer freq = random.nextInt(Integer.MAX_VALUE);
		assertThat(new Band(freq).value()).isEqualTo(freq);
	}
	@Test
	public void testToString() {
		assertThat(new Band(    1_900)).hasToString("1.9MHz");
		assertThat(new Band(    3_500)).hasToString("3.5MHz");
		assertThat(new Band(  430_000)).hasToString("430MHz");
		assertThat(new Band(5_600_000)).hasToString("5.6GHz");
	}
	@Test
	public void testToKHzString() {
		assertThat(new Band(1_900).toKHzString()).isEqualTo("1900kHz");
		assertThat(new Band(3_500).toKHzString()).isEqualTo("3500kHz");
		assertThat(new Band(7_000).toKHzString()).isEqualTo("7000kHz");
	}
	@Test
	public void testToMHzString() {
		assertThat(new Band(1_900).toMHzString()).isEqualTo("1.9MHz");
		assertThat(new Band(3_500).toMHzString()).isEqualTo("3.5MHz");
		assertThat(new Band(7_000).toMHzString()).isEqualTo(  "7MHz");
	}
	@Test
	public void testToGHzString() {
		assertThat(new Band(1_200_000).toGHzString()).isEqualTo("1.2GHz");
		assertThat(new Band(2_400_000).toGHzString()).isEqualTo("2.4GHz");
		assertThat(new Band(5_600_000).toGHzString()).isEqualTo("5.6GHz");
	}
	@Test
	public void testBand$Format() throws Exception {
		final Band.Format form = new Band.Format();
		final Band band = new Band(random.nextInt(Integer.MAX_VALUE));
		assertThat(form.decode(form.encode(band))).isEqualTo(band);
		assertThat(cache.field(form.encode(band))).isEqualTo(band);
	}
}
