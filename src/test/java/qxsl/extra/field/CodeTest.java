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
 * {@link Code}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 *
 */
public final class CodeTest extends test.RandTest {
	private final Cache cache = new FieldFormats().cache(Qxsl.CODE);
	@Test
	public void testValue() {
		assertThat(new Code("100110H").value()).isEqualTo("100110H");
		assertThat(new Code("400105M").value()).isEqualTo("400105M");
	}
	@Test
	public void testToString() {
		final String text = alnum(100);
		assertThat(new Code(text)).hasToString(text);
	}
	@Test
	public void testCode$Format() throws Exception {
		final Code.Format form = new Code.Format();
		final Code code = new Code(alnum(100));
		assertThat(form.decode(form.encode(code))).isEqualTo(code);
		assertThat(cache.field(form.encode(code))).isEqualTo(code);
	}
}
