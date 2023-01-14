/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.draft.Mul2;
import qxsl.draft.Qxsl;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Mul2Factory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/28
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class Mul2FactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.MUL2);

	@Test
	public void test(@RandomString String text) throws Exception {
		final var form = new Mul2Factory();
		final var code = new Mul2(text);
		assertThat(form.decode(form.encode(code))).isEqualTo(code);
		assertThat(cache.field(form.encode(code))).isEqualTo(code);
	}
}
