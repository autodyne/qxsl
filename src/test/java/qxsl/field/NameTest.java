/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import org.junit.Test;
import qxsl.table.Fields;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Name}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class NameTest extends junit.framework.TestCase {
	private final Fields.Cache cache = new Fields().cache(Qxsl.NAME);
	@Test
	public void testValue() {
		assertThat(new Name("筑波大").value()).isEqualTo("筑波大");
		assertThat(new Name("電通大").value()).isEqualTo("電通大");
	}
	@Test
	public void testToString() {
		final String text = util.RandText.alnum(100);
		assertThat(new Name(text)).hasToString(text);
	}
	@Test
	public void testName$Format() throws Exception {
		final Name.Format form = new Name.Format();
		final Name name = new Name(util.RandText.alnum(100));
		assertThat(form.decode(form.encode(name))).isEqualTo(name);
		assertThat(cache.field(form.encode(name))).isEqualTo(name);
	}
}
