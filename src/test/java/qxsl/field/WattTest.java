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
import static qxsl.table.secret.QxmlFormat.WATT;

/**
 * {@see Watt}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/24
 *
 */
public final class WattTest extends junit.framework.TestCase {
	private final Fields fields = new Fields(WATT);
	@Test
	public void testValue() {
		final String str = util.RandText.alnum(100);
		assertThat(new Watt(str).value(), is(str));
	}
	@Test
	public void testWatt$Format() throws Exception {
		final Watt.Format $form = new Watt.Format();
		final Watt watt = new Watt(util.RandText.alnum(100));
		assertThat($form.decode($form.encode(watt)), is(watt));
		assertThat(fields.cache($form.encode(watt)), is(watt));
	}
}
