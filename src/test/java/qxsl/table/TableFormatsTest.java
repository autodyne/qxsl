/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.table;

import org.junit.jupiter.api.Test;
import qxsl.extra.table.*;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link TableFormats}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 *
 */
public final class TableFormatsTest extends test.RandTest {
	private final TableFormats tables = new TableFormats();
	@Test
	public void testIterator() {
		assertThat(tables.iterator()).hasNext();
	}
	@Test
	public void testGetFormat() {
		// XML
		assertThat(tables.forName("qxml")).isInstanceOf(QxmlFormat.class);
		assertThat(tables.forName("adxs")).isInstanceOf(AdxsFormat.class);
		// text
		assertThat(tables.forName("adis")).isInstanceOf(AdisFormat.class);
		assertThat(tables.forName("cqww")).isInstanceOf(CqwwFormat.class);
		assertThat(tables.forName("jarl")).isInstanceOf(JarlFormat.class);
		assertThat(tables.forName("ctxt")).isInstanceOf(CTxtFormat.class);
		assertThat(tables.forName("zall")).isInstanceOf(ZAllFormat.class);
		assertThat(tables.forName("zdos")).isInstanceOf(ZDosFormat.class);
		// binary
		assertThat(tables.forName("cbin")).isInstanceOf(CBinFormat.class);
		assertThat(tables.forName("zbin")).isInstanceOf(ZBinFormat.class);
	}
}
