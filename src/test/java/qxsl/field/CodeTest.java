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
 * {@link Code}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class CodeTest extends junit.framework.TestCase {
	private final Fields.Cache cache = new Fields().cache(Qxsl.CODE);
	@Test
	public void testValue() {
		assertThat(new Code("100110H").value()).isEqualTo("100110H");
		assertThat(new Code("400105M").value()).isEqualTo("400105M");
	}
	@Test
	public void testToString() {
		final String text = util.RandText.alnum(100);
		assertThat(new Code(text)).hasToString(text);
	}
	@Test
	public void testCode$Format() throws Exception {
		final Code.Format form = new Code.Format();
		final Code code = new Code(util.RandText.alnum(100));
		assertThat(form.decode(form.encode(code))).isEqualTo(code);
		assertThat(cache.field(form.encode(code))).isEqualTo(code);
	}
}
