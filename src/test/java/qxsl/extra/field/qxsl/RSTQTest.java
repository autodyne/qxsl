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
 * {@link RSTQ}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class RSTQTest extends junit.framework.TestCase {
	private final Fields.Cache cache = new Fields().cache(Qxsl.RSTQ);
	private final Random random = new Random();
	@Test
	public void testValue() {
		assertThat(new RSTQ(699).value()).isEqualTo(599);
		assertThat(new RSTQ(599).value()).isEqualTo(599);
		assertThat(new RSTQ(590).value()).isEqualTo(591);
		assertThat(new RSTQ(101).value()).isEqualTo(111);
		assertThat(new RSTQ(100).value()).isEqualTo(111);
		assertThat(new RSTQ( 59).value()).isEqualTo( 59);
		assertThat(new RSTQ( 11).value()).isEqualTo( 11);
		assertThat(new RSTQ( 10).value()).isEqualTo( 11);
	}
	@Test
	public void testToString() {
		assertThat(new RSTQ(599)).hasToString("599");
		assertThat(new RSTQ(590)).hasToString("591");
		assertThat(new RSTQ(101)).hasToString("111");
		assertThat(new RSTQ(100)).hasToString("111");
		assertThat(new RSTQ( 59)).hasToString( "59");
	}
	@Test
	public void testRSTQ$Format() throws Exception {
		final RSTQ.Format form = new RSTQ.Format();
		final RSTQ rstq = new RSTQ(random.nextInt(489) + 111);
		assertThat(form.decode(form.encode(rstq))).isEqualTo(rstq);
		assertThat(cache.field(form.encode(rstq))).isEqualTo(rstq);
	}
}
