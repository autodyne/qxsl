/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.field;

import org.junit.jupiter.api.Test;
import qxsl.field.FieldFormats;
import qxsl.field.FieldFormats.Cache;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Call}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 *
 */
public final class CallTest extends test.RandTest {
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
	public void testToString() {
		final String text = alnum(100).toUpperCase();
		assertThat(new Call(text)).hasToString(text);
	}
	@Test
	public void testCall$Format() throws Exception {
		final Call.Format form = new Call.Format();
		final Call call = new Call(alnum(100));
		assertThat(form.decode(form.encode(call))).isEqualTo(call);
		assertThat(cache.field(form.encode(call))).isEqualTo(call);
	}
}
