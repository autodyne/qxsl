/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.extra.table.*;

/**
 * {@link TableManager}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class TableFormatsTest extends Assertions {
	private final TableManager tables = new TableManager();

	@Test
	public void testIterator() {
		assertThat(tables.iterator()).hasNext();
	}

	@Test
	public void testGetFormat() {
		// XML
		assertThat(tables.forName("qxml")).isInstanceOf(QxmlFactory.class);
		assertThat(tables.forName("adxs")).isInstanceOf(AdxsFactory.class);
		// text
		assertThat(tables.forName("adis")).isInstanceOf(AdisFactory.class);
		assertThat(tables.forName("cqww")).isInstanceOf(CqwwFactory.class);
		assertThat(tables.forName("jarl")).isInstanceOf(JarlFactory.class);
		assertThat(tables.forName("ctxt")).isInstanceOf(CTxtFactory.class);
		assertThat(tables.forName("zall")).isInstanceOf(ZAllFactory.class);
		assertThat(tables.forName("zdos")).isInstanceOf(ZDosFactory.class);
		// binary
		assertThat(tables.forName("cbin")).isInstanceOf(CBinFactory.class);
		assertThat(tables.forName("zbin")).isInstanceOf(ZBinFactory.class);
	}
}
