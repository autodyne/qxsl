/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.draft.Qxsl;
import qxsl.draft.RSTQ;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomNumberParameterExtension;
import qxsl.junit.RandomNumberParameterExtension.RandomNumber;

/**
 * {@link RSTQ}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomNumberParameterExtension.class)
public final class RSTQTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.RSTQ);

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
		assertThat(new RSTQ( 59)).hasToString("59");
	}

	@Test
	public void testRSTQ$Format(@RandomNumber(489) int num) throws Exception {
		final var form = new RSTQFactory();
		final var rstq = new RSTQ(num + 111);
		assertThat(form.decode(form.encode(rstq))).isEqualTo(rstq);
		assertThat(cache.field(form.encode(rstq))).isEqualTo(rstq);
	}
}
