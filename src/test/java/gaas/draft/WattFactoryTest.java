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
import qxsl.draft.Watt;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link WattFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class WattFactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.WATT);

	@Test
	public void test(@RandomString String text) throws Exception {
		final var form = new WattFactory();
		final var watt = new Watt(text);
		assertThat(form.decode(form.encode(watt))).isEqualTo(watt);
		assertThat(cache.field(form.encode(watt))).isEqualTo(watt);
	}
}
