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
		assertThat(tables.factory("qxml")).isInstanceOf(QxmlFactory.class);
		assertThat(tables.factory("adxs")).isInstanceOf(AdxsFactory.class);
		assertThat(tables.factory("adis")).isInstanceOf(AdisFactory.class);
		assertThat(tables.factory("cqww")).isInstanceOf(CqwwFactory.class);
		assertThat(tables.factory("jarl")).isInstanceOf(JarlFactory.class);
		assertThat(tables.factory("ctxt")).isInstanceOf(CTxtFactory.class);
		assertThat(tables.factory("zall")).isInstanceOf(ZAllFactory.class);
		assertThat(tables.factory("zdos")).isInstanceOf(ZDosFactory.class);
		assertThat(tables.factory("cbin")).isInstanceOf(CBinFactory.class);
		assertThat(tables.factory("zbin")).isInstanceOf(ZBinFactory.class);
	}
}
