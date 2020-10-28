/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.draft.Code;
import qxsl.draft.Qxsl;
import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link CodeFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class CodeFactoryTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.CODE);

	@Test
	public void test(@RandomString String text) throws Exception {
		final var form = new CodeFactory();
		final var code = new Code(text);
		assertThat(form.decode(form.encode(code))).isEqualTo(code);
		assertThat(cache.field(form.encode(code))).isEqualTo(code);
	}
}
