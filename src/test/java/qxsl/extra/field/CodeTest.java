/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.field.FieldManager;
import qxsl.field.FieldManager.Cache;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Code}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class CodeTest extends Assertions {
	private final Cache cache = new FieldManager().cache(Qxsl.CODE);

	@Test
	public void testValue() {
		assertThat(new Code("100110H").value()).isEqualTo("100110H");
		assertThat(new Code("400105M").value()).isEqualTo("400105M");
	}

	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Code(text)).hasToString(text);
	}

	@Test
	public void testCode$Format(@RandomString String text) throws Exception {
		final var form = new Code.Factory();
		final var code = new Code(text);
		assertThat(form.decode(form.encode(code))).isEqualTo(code);
		assertThat(cache.field(form.encode(code))).isEqualTo(code);
	}
}
