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
import static qxsl.table.secret.QxmlFormat.CODE;

/**
 * {@see Code}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class CodeTest extends junit.framework.TestCase {
	private final Fields fields = new Fields(CODE);
	@Test
	public void testValue() {
		final String str = randomAlphanumeric(1, 100);
		assertThat(new Code(str).value(), is(str));
	}
	@Test
	public void testCode$Format() throws Exception {
		final Code.Format $form = new Code.Format();
		final Code code = new Code(randomAlphanumeric(1, 100));
		assertThat($form.decode($form.encode(code)), is(code));
		assertThat(fields.cache($form.encode(code)), is(code));
	}
}
