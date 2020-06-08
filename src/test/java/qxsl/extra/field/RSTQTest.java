/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import org.junit.jupiter.api.Test;
import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;

import static test.RandTest.*;

/**
 * {@link RSTQ}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 *
 */
public final class RSTQTest extends org.assertj.core.api.Assertions {
	private final Cache cache = new FieldFormats().cache(Qxsl.RSTQ);
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
		final RSTQ rstq = new RSTQ(randInt(489) + 111);
		assertThat(form.decode(form.encode(rstq))).isEqualTo(rstq);
		assertThat(cache.field(form.encode(rstq))).isEqualTo(rstq);
	}
}
