/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.table;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import gaas.table.*;

/**
 * {@link TableManager}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class TableManagerTest extends Assertions {
	private final TableManager tables = new TableManager();

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

	@Test
	public void testIterator() {
		assertThat(tables.iterator()).hasNext();
	}
}
