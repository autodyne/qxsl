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
		final Integer band = random.nextInt(Integer.MAX_VALUE);
		assertThat(new Band(band).value()).isEqualTo(band);
	}
	@Test
	public void testToString() {
		assertThat(new Band(2190)).hasToString("2190m");
		assertThat(new Band(1.25)).hasToString("1.25m");
		assertThat(new Band(0.06)).hasToString("0.06m");
	}
	@Test
	public void testBand$Format() throws Exception {
		final Band.Format form = new Band.Format();
		final Band band = new Band(random.nextInt(Integer.MAX_VALUE));
		assertThat(form.decode(form.encode(band))).isEqualTo(band);
		assertThat(cache.field(form.encode(band))).isEqualTo(band);
	}
}
