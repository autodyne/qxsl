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
 * {@link Name}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 *
 */
public final class NameTest extends test.RandTest {
	private final Cache cache = new FieldFormats().cache(Qxsl.NAME);
	@Test
	public void testValue() {
		assertThat(new Name("筑波大").value()).isEqualTo("筑波大");
		assertThat(new Name("電通大").value()).isEqualTo("電通大");
	}
	@Test
	public void testToString() {
		final String text = alnum(100);
		assertThat(new Name(text)).hasToString(text);
	}
	@Test
	public void testName$Format() throws Exception {
		final Name.Format form = new Name.Format();
		final Name name = new Name(alnum(100));
		assertThat(form.decode(form.encode(name))).isEqualTo(name);
		assertThat(cache.field(form.encode(name))).isEqualTo(name);
	}
}
