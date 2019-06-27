/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table;

import org.junit.Test;
import qxsl.extra.table.*;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Tables}クラスのテスト用クラスです。
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
		assertThat(tables.iterator()).hasNext();
	}
	@Test
	public void testGetFormat() {
		// XML format
		assertThat(tables.getFormat("qxml")).isInstanceOf(QxmlFormat.class);
		// Text formats
		assertThat(tables.getFormat("cqww")).isInstanceOf(CqwwFormat.class);
		assertThat(tables.getFormat("jarl")).isInstanceOf(JarlFormat.class);
		assertThat(tables.getFormat("ctxt")).isInstanceOf(CTxtFormat.class);
		assertThat(tables.getFormat("hl76")).isInstanceOf(Hl76Format.class);
		assertThat(tables.getFormat("rtcl")).isInstanceOf(RtclFormat.class);
		assertThat(tables.getFormat("zall")).isInstanceOf(ZAllFormat.class);
		assertThat(tables.getFormat("zdos")).isInstanceOf(ZDosFormat.class);
		// Binary formats
		assertThat(tables.getFormat("cbin")).isInstanceOf(CBinFormat.class);
		assertThat(tables.getFormat("zbin")).isInstanceOf(ZBinFormat.class);
	}
}
