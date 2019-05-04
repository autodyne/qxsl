/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.sheet;

import org.junit.Test;
import qxsl.sheet.secret.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

/**
 * {@see Sheets}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/03/12
 *
 */
public final class SheetsTest extends junit.framework.TestCase {
	private final Sheets sheets = new Sheets();
	@Test
	public void testIterator() {
		assertThat(sheets.iterator(), is(notNullValue()));
		assertThat(sheets.iterator().hasNext(), is(true));
	}
	@Test
	public void testGetFormat() {
		assertThat(sheets.getFormat("cab3"), is(instanceOf(Cab3Format.class)));
		assertThat(sheets.getFormat("jarl"), is(instanceOf(JarlFormat.class)));
	}
}
