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
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.QxmlFormat.NAME;

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
	private final Fields fields = new Fields(NAME);
	@Test
	public void testValue() {
		final String str = util.RandText.alnum(100);
		assertThat(new Name(str).value(), is(str));
	}
	@Test
	public void testName$Format() throws Exception {
		final Name.Format $form = new Name.Format();
		final Name name = new Name(util.RandText.alnum(100));
		assertThat($form.decode($form.encode(name)), is(name));
		assertThat(fields.cache($form.encode(name)), is(name));
	}
}
