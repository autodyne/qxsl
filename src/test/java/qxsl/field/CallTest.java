/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import org.junit.Test;
import qxsl.model.Fields;
import static org.apache.commons.lang3.RandomStringUtils.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.QxmlFormat.CALL;

/**
 * {@see Call}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class CallTest extends junit.framework.TestCase {
	private final Fields fields = new Fields(CALL);
	@Test
	public void testValue() {
		assertThat(new Call("JA1ZLO").value(), is("JA1ZLO"));
		assertThat(new Call("JA1YWX").value(), is("JA1YWX"));
	}
	@Test
	public void testCall$Format() throws Exception {
		final Call.Format $form = new Call.Format();
		final Call call = new Call(randomAlphanumeric(1, 100));
		assertThat($form.decode($form.encode(call)), is(call));
		assertThat(fields.cache($form.encode(call)), is(call));
	}
}
