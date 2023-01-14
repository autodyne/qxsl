/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.draft.Band;
import qxsl.draft.Qxsl;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomNumberParameterExtension;
import qxsl.junit.RandomNumberParameterExtension.RandomNumber;

/**
 * {@link BandFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomNumberParameterExtension.class)
public final class BandFactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.BAND);

	@Test
	public void test(@RandomNumber int num) throws Exception {
		final var form = new BandFactory();
		final var band = new Band(num);
		assertThat(form.decode(form.encode(band))).isEqualTo(band);
		assertThat(cache.field(form.encode(band))).isEqualTo(band);
	}
}
