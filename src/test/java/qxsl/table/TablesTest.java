/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table;

import org.junit.Test;
import qxsl.table.secret.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

/**
 * {@see Tables}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class TablesTest extends junit.framework.TestCase {
	private final Tables tables = new Tables();
	@Test
	public void testIterator() {
		assertThat(tables.iterator(), is(notNullValue()));
		assertThat(tables.iterator().hasNext(), is(true));
	}
	@Test
	public void testGetFormat() {
		// XML format
		assertThat(tables.getFormat("qxml"), is(instanceOf(QxmlFormat.class)));
		// Text formats
		assertThat(tables.getFormat("cab3"), is(instanceOf(Cab3Format.class)));
		assertThat(tables.getFormat("jarl"), is(instanceOf(JarlFormat.class)));
		assertThat(tables.getFormat("ctxt"), is(instanceOf(CTxtFormat.class)));
		assertThat(tables.getFormat("hl76"), is(instanceOf(Hl76Format.class)));
		assertThat(tables.getFormat("rtcl"), is(instanceOf(RtclFormat.class)));
		assertThat(tables.getFormat("zall"), is(instanceOf(ZAllFormat.class)));
		assertThat(tables.getFormat("zdos"), is(instanceOf(ZDosFormat.class)));
		// Binary formats
		assertThat(tables.getFormat("cbin"), is(instanceOf(CBinFormat.class)));
		assertThat(tables.getFormat("zbin"), is(instanceOf(ZBinFormat.class)));
	}
}
