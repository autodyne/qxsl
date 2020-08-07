/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Call}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class CallTest extends org.assertj.core.api.Assertions {
	private final Cache cache = new FieldFormats().cache(Qxsl.CALL);

	@Test
	public void testValue() {
		assertThat(new Call("JA1ZLO").value()).isEqualTo("JA1ZLO");
		assertThat(new Call("JA1YWX").value()).isEqualTo("JA1YWX");
	}

	@Test
	public void testStrip() {
		assertThat(new Call("JA1ZLO/1").strip()).isEqualTo("JA1ZLO");
		assertThat(new Call("JA1YWX/2").strip()).isEqualTo("JA1YWX");
	}

	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Call(text)).hasToString(text.toUpperCase());
	}

	@Test
	public void testCall$Format(@RandomString String text) throws Exception {
		final var form = new Call.Format();
		final var call = new Call(text);
		assertThat(form.decode(form.encode(call))).isEqualTo(call);
		assertThat(cache.field(form.encode(call))).isEqualTo(call);
	}
}
