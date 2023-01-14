/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.sheet;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import gaas.sheet.Cab3Factory;
import gaas.sheet.JarlFactory;

/**
 * {@link SheetManager}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/12
 */
public final class SheetManagerTest extends Assertions {
	private final SheetManager sheets = new SheetManager();

	@Test
	public void testGetFactory() {
		assertThat(sheets.factory("cab3")).isInstanceOf(Cab3Factory.class);
		assertThat(sheets.factory("jarl")).isInstanceOf(JarlFactory.class);
	}

	@Test
	public void testIterator() {
		assertThat(sheets.iterator()).hasNext();
	}
}
