/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
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
 * {@link RSTQFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomNumberParameterExtension.class)
public final class RSTQFactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.RSTQ);

	@Test
	public void test(@RandomNumber(489) int num) throws Exception {
		final var form = new RSTQFactory();
		final var rstq = new RSTQ(num + 111);
		assertThat(form.decode(form.encode(rstq))).isEqualTo(rstq);
		assertThat(cache.field(form.encode(rstq))).isEqualTo(rstq);
	}
}
