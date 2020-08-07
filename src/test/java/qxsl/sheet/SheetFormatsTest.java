/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import org.junit.jupiter.api.Test;
import qxsl.extra.sheet.Cab3Format;
import qxsl.extra.sheet.JarlFormat;

/**
 * {@link SheetFormats}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/12
 */
public final class SheetFormatsTest extends org.assertj.core.api.Assertions {
	private final SheetFormats sheets = new SheetFormats();

	@Test
	public void testIterator() {
		assertThat(sheets.iterator()).hasNext();
	}

	@Test
	public void testGetFormat() {
		assertThat(sheets.forName("cab3")).isInstanceOf(Cab3Format.class);
		assertThat(sheets.forName("jarl")).isInstanceOf(JarlFormat.class);
	}
}
