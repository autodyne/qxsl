/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field;

import java.util.Random;
import org.junit.jupiter.api.Test;
import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Band}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/29
 *
 */
public final class BandTest extends junit.framework.TestCase {
	private final Cache cache = new FieldFormats().cache(Qxsl.BAND);
	private final Random random = new Random();
	@Test
	public void testValue() {
		assertThat(new Band("21900m")).isEqualTo(new Band(2190e+1));
		assertThat(new Band("21.9cm")).isEqualTo(new Band(2.19e-1));
		assertThat(new Band("2.19mm")).isEqualTo(new Band(2.19e-3));
	}
	@Test
	public void testToString() {
		assertThat(new Band(21.9e+3)).hasToString("21900m");
		assertThat(new Band(21.9e-2)).hasToString("21.9cm");
		assertThat(new Band(2.19e-3)).hasToString("2.19mm");
	}
	@Test
	public void testToMString() {
		assertThat(new Band(2190).toMString()).isEqualTo("2190m");
		assertThat(new Band(2.19).toMString()).isEqualTo("2.19m");
	}
	@Test
	public void testToCMtring() {
		assertThat(new Band(0.006).toCMString()).isEqualTo("0.6cm");
		assertThat(new Band(6.000).toCMString()).isEqualTo("600cm");
	}
	@Test
	public void testToMMString() {
		assertThat(new Band(0.0006).toMMString()).isEqualTo("0.6mm");
		assertThat(new Band(0.6000).toMMString()).isEqualTo("600mm");
	}
	@Test
	public void testBand$Format() throws Exception {
		final Band.Format form = new Band.Format();
		final Band band = new Band(random.nextInt(Integer.MAX_VALUE));
		assertThat(form.decode(form.encode(band))).isEqualTo(band);
		assertThat(cache.field(form.encode(band))).isEqualTo(band);
	}
}
