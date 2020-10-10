/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.table;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import gaas.table.*;

/**
 * {@link TableManager}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class TableManagerTest extends Assertions {
	private final TableManager tables = new TableManager();

	@Test
	public void testIterator() {
		assertThat(tables.iterator()).hasNext();
	}

	@Test
	public void testGetFactory() {
		// XML
		assertThat(tables.getFactory("qxml")).isInstanceOf(QxmlFactory.class);
		assertThat(tables.getFactory("adxs")).isInstanceOf(AdxsFactory.class);
		// text
		assertThat(tables.getFactory("adis")).isInstanceOf(AdisFactory.class);
		assertThat(tables.getFactory("cqww")).isInstanceOf(CqwwFactory.class);
		assertThat(tables.getFactory("jarl")).isInstanceOf(JarlFactory.class);
		assertThat(tables.getFactory("ctxt")).isInstanceOf(CTxtFactory.class);
		assertThat(tables.getFactory("zall")).isInstanceOf(ZAllFactory.class);
		assertThat(tables.getFactory("zdos")).isInstanceOf(ZDosFactory.class);
		// binary
		assertThat(tables.getFactory("cbin")).isInstanceOf(CBinFactory.class);
		assertThat(tables.getFactory("zbin")).isInstanceOf(ZBinFactory.class);
	}
}
