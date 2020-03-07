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
 * {@link Mode}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 *
 */
public final class ModeTest extends test.RandTest {
	private final Cache cache = new FieldFormats().cache(Qxsl.MODE);
	@Test
	public void testValue() {
		assertThat(new Mode("CW").value()).isEqualTo("CW");
		assertThat(new Mode("AM").value()).isEqualTo("AM");
	}
	@Test
	public void testToString() {
		final String text = alnum(100);
		assertThat(new Mode(text)).hasToString(text);
	}
	@Test
	public void testMode$Format() throws Exception {
		final Mode.Format form = new Mode.Format();
		final Mode mode = new Mode(alnum(100));
		assertThat(form.decode(form.encode(mode))).isEqualTo(mode);
		assertThat(cache.field(form.encode(mode))).isEqualTo(mode);
	}
}
