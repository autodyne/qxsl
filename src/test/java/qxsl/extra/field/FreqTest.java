/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field;

import java.util.Random;
import org.junit.Test;
import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Freq}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class FreqTest extends junit.framework.TestCase {
	private final Cache cache = new FieldFormats().cache(Qxsl.FREQ);
	private final Random random = new Random();
	@Test
	public void testValue() {
		final Integer freq = random.nextInt(Integer.MAX_VALUE);
		assertThat(new Freq(freq).value()).isEqualTo(freq);
	}
	@Test
	public void testToString() {
		assertThat(new Freq(    1_900)).hasToString("1.9MHz");
		assertThat(new Freq(    3_500)).hasToString("3.5MHz");
		assertThat(new Freq(  430_000)).hasToString("430MHz");
		assertThat(new Freq(5_600_000)).hasToString("5.6GHz");
	}
	@Test
	public void testToKHzString() {
		assertThat(new Freq(1_900).toKHzString()).isEqualTo("1900kHz");
		assertThat(new Freq(3_500).toKHzString()).isEqualTo("3500kHz");
		assertThat(new Freq(7_000).toKHzString()).isEqualTo("7000kHz");
	}
	@Test
	public void testToMHzString() {
		assertThat(new Freq(1_900).toMHzString()).isEqualTo("1.9MHz");
		assertThat(new Freq(3_500).toMHzString()).isEqualTo("3.5MHz");
		assertThat(new Freq(7_000).toMHzString()).isEqualTo(  "7MHz");
	}
	@Test
	public void testToGHzString() {
		assertThat(new Freq(1_200_000).toGHzString()).isEqualTo("1.2GHz");
		assertThat(new Freq(2_400_000).toGHzString()).isEqualTo("2.4GHz");
		assertThat(new Freq(5_600_000).toGHzString()).isEqualTo("5.6GHz");
	}
	@Test
	public void testFreq$Format() throws Exception {
		final Freq.Format form = new Freq.Format();
		final Freq freq = new Freq(random.nextInt(Integer.MAX_VALUE));
		assertThat(form.decode(form.encode(freq))).isEqualTo(freq);
		assertThat(cache.field(form.encode(freq))).isEqualTo(freq);
	}
}
