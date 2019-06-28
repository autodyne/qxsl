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
 * {@link Call}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class CallTest extends junit.framework.TestCase {
	private final Fields.Cache cache = new Fields().cache(Qxsl.CALL);
	@Test
	public void testValue() {
		assertThat(new Call("JA1ZLO").value()).isEqualTo("JA1ZLO");
		assertThat(new Call("JA1YWX").value()).isEqualTo("JA1YWX");
	}
	@Test
	public void testToString() {
		final String text = util.RandText.alnum(100);
		assertThat(new Call(text)).hasToString(text);
	}
	@Test
	public void testCall$Format() throws Exception {
		final Call.Format form = new Call.Format();
		final Call call = new Call(util.RandText.alnum(100));
		assertThat(form.decode(form.encode(call))).isEqualTo(call);
		assertThat(cache.field(form.encode(call))).isEqualTo(call);
	}
}
