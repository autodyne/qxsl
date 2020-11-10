/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import gaas.sheet.Cab3Factory;
import gaas.sheet.JarlFactory;

/**
 * {@link SheetManager}クラスのテスト用クラスです。
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
