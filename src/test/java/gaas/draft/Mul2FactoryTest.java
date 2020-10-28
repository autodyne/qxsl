/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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
 * {@link Mul2Factory}クラスのテスト用クラスです。
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
